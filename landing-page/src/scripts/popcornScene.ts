import { WebGLRenderer } from "three";
import * as THREE from "three/webgpu";
import { GLTFLoader } from "three/addons/loaders/GLTFLoader.js";

type Renderer = {
  domElement: HTMLCanvasElement;
  setPixelRatio(ratio: number): void;
  setSize(width: number, height: number, updateStyle?: boolean): void;
  render(scene: THREE.Scene, camera: THREE.Camera): unknown;
  dispose(): void;
  forceContextLoss?(): void;
};

const AUTO_ROTATION_SPEED = 0.08;
const ENTRANCE_DURATION = 0.9;
const DRAG_ROTATION = 0.004;
const MAX_MOMENTUM = 5;
const MOMENTUM_DECAY = 0.5;
const DRAG_X_AXIS = new THREE.Vector3(0, 1, 0);
const DRAG_Y_AXIS = new THREE.Vector3(1, 0, 0);

export type PopcornRuntime = {
  setVisibility(visible: boolean[]): void;
  setPageVisible(visible: boolean): void;
  dispose(): void;
};

export async function createPopcornRuntime(
  elements: HTMLElement[],
  signal: AbortSignal,
): Promise<PopcornRuntime> {
  assert(elements.length === 2, "Hero requires two popcorn views");

  const canvases = elements.map(() => document.createElement("canvas"));
  for (const [index, canvas] of canvases.entries()) {
    canvas.setAttribute("aria-hidden", "true");
    canvas.className = "block size-full";
    elements[index].append(canvas);
  }

  const rendererPromise = Promise.all(canvases.map(createRenderer));
  const modelPromise = fetch("/popcorn.glb", {
    cache: "force-cache",
    signal,
  }).then(async (response) => {
    assert(response.ok, `Model request failed with ${response.status}`);
    const data = await response.arrayBuffer();
    return new GLTFLoader().parseAsync(data, "/");
  });

  let rendererResults: Awaited<typeof rendererPromise>;
  let gltf: Awaited<typeof modelPromise>;
  try {
    [rendererResults, gltf] = await Promise.all([
      rendererPromise,
      modelPromise,
    ]);
  } catch (error) {
    const settledRenderers = await rendererPromise.catch(() => []);
    for (const renderer of settledRenderers) renderer.dispose();
    for (const element of elements) element.querySelector("canvas")?.remove();
    throw error;
  }

  if (signal.aborted) {
    for (const renderer of rendererResults) renderer.dispose();
    disposeScene(gltf.scene);
    for (const element of elements) element.querySelector("canvas")?.remove();
    throw signal.reason;
  }

  const pixelRatio = boundedPixelRatio();
  const views = elements.map((element, index) =>
    createView(
      element,
      rendererResults[index],
      gltf.scene,
      index === 1,
      pixelRatio,
    ),
  );
  const resizeObserver = new ResizeObserver(() => {
    for (const view of views) view.resize();
    schedule();
  });
  for (const element of elements) resizeObserver.observe(element);

  let disposed = false;
  let pageVisible = !document.hidden;
  let frame = 0;
  let lastRender = 0;

  function isActive() {
    return (
      !disposed &&
      pageVisible &&
      views.some((view) => view.visible && view.needsFrame())
    );
  }

  function schedule() {
    if (frame !== 0 || !isActive()) return;
    frame = requestAnimationFrame(tick);
  }

  function tick(now: number) {
    frame = 0;
    if (!isActive()) return;

    const highFrequency = views.some(
      (view) => view.visible && view.highFrequency,
    );
    const interval = highFrequency ? 16 : 33;
    if (lastRender !== 0 && now - lastRender < interval) {
      schedule();
      return;
    }

    const previousRender = lastRender === 0 ? now - interval : lastRender;
    const delta = Math.min((now - previousRender) / 1000, 0.1);
    lastRender = now;
    for (const view of views) {
      if (!view.visible || !view.needsFrame()) continue;
      view.update(delta);
      view.render();
    }
    schedule();
  }

  for (const view of views) view.connect(() => schedule());
  for (const view of views) view.resize();

  return {
    setVisibility(visible) {
      assert(visible.length === views.length, "Invalid visibility state");
      for (const [index, view] of views.entries())
        view.setVisible(visible[index]);
      if (visible.some(Boolean)) lastRender = performance.now();
      schedule();
    },
    setPageVisible(visible) {
      pageVisible = visible;
      if (!visible) cancelAnimationFrame(frame);
      frame = 0;
      if (visible) {
        lastRender = performance.now();
        for (const view of views) view.invalidate();
        schedule();
      }
    },
    dispose() {
      if (disposed) return;
      disposed = true;
      cancelAnimationFrame(frame);
      resizeObserver.disconnect();
      disposeScene(gltf.scene);
      for (const view of views) view.dispose();
    },
  };
}

async function createRenderer(canvas: HTMLCanvasElement) {
  if ("gpu" in navigator) {
    const candidate = new THREE.WebGPURenderer({
      alpha: true,
      antialias: false,
      canvas,
      powerPreference: "high-performance",
    });
    try {
      await candidate.init();
    } catch {
      candidate.dispose();
      return createWebGLRenderer(replaceCanvas(canvas));
    }
    if ((candidate.backend as { isWebGPUBackend?: boolean }).isWebGPUBackend) {
      return candidate as Renderer;
    }
    candidate.dispose();
    return createWebGLRenderer(replaceCanvas(canvas));
  }
  return createWebGLRenderer(canvas);
}

function createWebGLRenderer(canvas: HTMLCanvasElement) {
  const renderer = new WebGLRenderer({
    alpha: true,
    antialias: false,
    canvas,
    powerPreference: "high-performance",
  });
  return renderer as Renderer;
}

function replaceCanvas(canvas: HTMLCanvasElement) {
  const replacement = canvas.cloneNode() as HTMLCanvasElement;
  canvas.replaceWith(replacement);
  return replacement;
}

function createView(
  element: HTMLElement,
  renderer: Renderer,
  source: THREE.Group,
  flipped: boolean,
  pixelRatio: number,
) {
  const scene = new THREE.Scene();
  const camera = new THREE.PerspectiveCamera(35, 1, 0.1, 100);
  const model = frameModel(source.clone(true), camera, flipped ? 0.62 : 0.55);
  if (flipped) model.rotation.z = Math.PI;
  const entrance = new THREE.Group();
  entrance.scale.setScalar(0);
  entrance.add(model);
  scene.add(entrance);
  scene.add(
    new THREE.HemisphereLight(
      color("--color-cream"),
      color("--color-brown-70"),
      2.4,
    ),
  );
  const keyLight = new THREE.DirectionalLight(color("--color-cream"), 2.6);
  keyLight.position.set(4, 6, 8);
  scene.add(keyLight);

  const interaction = createInteraction(
    element,
    scene,
    model,
    camera,
    flipped ? -1 : 1,
  );
  let visible = false;
  let dirty = true;
  let entranceTime = 0;
  let wake = () => {};

  return {
    get visible() {
      return visible;
    },
    get highFrequency() {
      return entranceTime < ENTRANCE_DURATION || interaction.highFrequency;
    },
    connect(onWake: () => void) {
      wake = onWake;
      interaction.connect(() => {
        dirty = true;
        wake();
      });
    },
    setVisible(next: boolean) {
      visible = next;
      interaction.setEnabled(next);
      if (next) dirty = true;
    },
    invalidate() {
      dirty = true;
    },
    resize() {
      const { width, height } = element.getBoundingClientRect();
      assert(width > 0 && height > 0, "Popcorn view has no size");
      renderer.setPixelRatio(pixelRatio);
      renderer.setSize(width, height, false);
      camera.aspect = width / height;
      camera.updateProjectionMatrix();
      dirty = true;
    },
    needsFrame() {
      return (
        dirty || entranceTime < ENTRANCE_DURATION || interaction.needsFrame()
      );
    },
    update(delta: number) {
      if (entranceTime < ENTRANCE_DURATION) {
        entranceTime = Math.min(entranceTime + delta, ENTRANCE_DURATION);
        const progress = entranceTime / ENTRANCE_DURATION;
        entrance.scale.setScalar(1 - (1 - progress) ** 3);
      }
      interaction.update(delta);
    },
    render() {
      renderer.render(scene, camera);
      dirty = false;
    },
    dispose() {
      interaction.dispose();
      renderer.dispose();
      renderer.forceContextLoss?.();
      renderer.domElement.remove();
    },
  };
}

function createInteraction(
  element: HTMLElement,
  scene: THREE.Scene,
  model: THREE.Group,
  camera: THREE.Camera,
  autoDirection: -1 | 1,
) {
  const abortController = new AbortController();
  const reducedMotion = matchMedia("(prefers-reduced-motion: reduce)");
  const kernels = model.getObjectByName("popcorn_grp")?.children ?? [];
  assert(kernels.length === 6, "Expected six popcorn kernels");

  const bases = new Map(
    kernels.map((kernel) => [
      kernel,
      {
        position: kernel.position.clone(),
        scale: kernel.scale.clone(),
        direction: kernel.position.clone().normalize(),
        influence: 0,
      },
    ]),
  );
  const raycaster = new THREE.Raycaster();
  let hovering = false;
  let pointerId: number | null = null;
  let enabled = false;
  let dragging = false;
  let velocityX = 0;
  let velocityY = 0;
  let previous = { x: 0, y: 0, time: 0 };
  let wake = () => {};

  function pointAt(event: PointerEvent) {
    if (reducedMotion.matches || event.pointerType === "touch") return;
    const bounds = element.getBoundingClientRect();
    const pointer = new THREE.Vector2(
      ((event.clientX - bounds.left) / bounds.width) * 2 - 1,
      -((event.clientY - bounds.top) / bounds.height) * 2 + 1,
    );
    scene.updateMatrixWorld(true);
    raycaster.setFromCamera(pointer, camera);
    const next = raycaster.intersectObjects(kernels, true).length > 0;
    if (next === hovering) return;
    hovering = next;
    wake();
  }

  function isWithinView(event: PointerEvent) {
    const bounds = element.getBoundingClientRect();
    return (
      event.clientX >= bounds.left &&
      event.clientX <= bounds.right &&
      event.clientY >= bounds.top &&
      event.clientY <= bounds.bottom
    );
  }

  function finish(event: PointerEvent) {
    if (event.pointerId !== pointerId) return;
    dragging = false;
    pointerId = null;
    if (element.hasPointerCapture(event.pointerId))
      element.releasePointerCapture(event.pointerId);
    if (event.pointerType === "touch" || !isWithinView(event)) hovering = false;
    wake();
  }

  return {
    get highFrequency() {
      return (
        dragging ||
        Math.abs(velocityX) > 0.005 ||
        Math.abs(velocityY) > 0.005 ||
        hoverIsSettling()
      );
    },
    connect(onWake: () => void) {
      wake = onWake;
      element.addEventListener(
        "pointerdown",
        (event) => {
          if (!enabled) return;
          pointerId = event.pointerId;
          dragging = true;
          velocityX = 0;
          velocityY = 0;
          previous = {
            x: event.clientX,
            y: event.clientY,
            time: event.timeStamp,
          };
          element.setPointerCapture(event.pointerId);
          pointAt(event);
          wake();
        },
        { signal: abortController.signal },
      );
      element.addEventListener(
        "pointermove",
        (event) => {
          if (!enabled) return;
          if (!isWithinView(event)) {
            finish(event);
            return;
          }
          pointAt(event);
          if (!dragging || event.pointerId !== pointerId) return;
          const seconds = Math.max(
            (event.timeStamp - previous.time) / 1000,
            1 / 120,
          );
          const x = (event.clientX - previous.x) * DRAG_ROTATION;
          const y = (event.clientY - previous.y) * DRAG_ROTATION;
          velocityX = THREE.MathUtils.clamp(
            x / seconds,
            -MAX_MOMENTUM,
            MAX_MOMENTUM,
          );
          velocityY = THREE.MathUtils.clamp(
            y / seconds,
            -MAX_MOMENTUM,
            MAX_MOMENTUM,
          );
          rotateInScreenSpace(model, x, y);
          previous = {
            x: event.clientX,
            y: event.clientY,
            time: event.timeStamp,
          };
          wake();
        },
        { signal: abortController.signal },
      );
      element.addEventListener("pointerup", finish, {
        signal: abortController.signal,
      });
      element.addEventListener("pointercancel", finish, {
        signal: abortController.signal,
      });
      element.addEventListener(
        "pointerleave",
        (event) => {
          if (dragging) {
            finish(event);
            return;
          }
          if (!hovering) return;
          hovering = false;
          wake();
        },
        { signal: abortController.signal },
      );
    },
    setEnabled(next: boolean) {
      enabled = next;
      if (next) return;
      if (pointerId !== null && element.hasPointerCapture(pointerId))
        element.releasePointerCapture(pointerId);
      dragging = false;
      pointerId = null;
      hovering = false;
    },
    needsFrame() {
      if (!reducedMotion.matches) return true;
      if (dragging) return true;
      if (Math.abs(velocityX) > 0.005 || Math.abs(velocityY) > 0.005)
        return true;
      return hoverIsSettling();
    },
    update(delta: number) {
      if (reducedMotion.matches) {
        velocityX = 0;
        velocityY = 0;
      } else {
        rotateInScreenSpace(
          model,
          AUTO_ROTATION_SPEED * autoDirection * delta,
          -AUTO_ROTATION_SPEED * delta,
        );
        if (!dragging) {
          rotateInScreenSpace(model, velocityX * delta, velocityY * delta);
          const decay = Math.exp(-MOMENTUM_DECAY * delta);
          velocityX *= decay;
          velocityY *= decay;
        }
      }

      for (const [kernel, base] of bases) {
        const target = hovering && !reducedMotion.matches ? 1 : 0;
        base.influence = THREE.MathUtils.lerp(
          base.influence,
          target,
          1 - Math.exp(-10 * delta),
        );
        kernel.position
          .copy(base.position)
          .addScaledVector(base.direction, base.influence * 0.08);
        kernel.scale
          .copy(base.scale)
          .multiplyScalar(1 + base.influence * 0.012);
      }
    },
    dispose() {
      abortController.abort();
    },
  };

  function hoverIsSettling() {
    const target = hovering && !reducedMotion.matches ? 1 : 0;
    for (const base of bases.values()) {
      if (Math.abs(base.influence - target) > 0.002) return true;
    }
    return false;
  }
}

function rotateInScreenSpace(
  model: THREE.Group,
  horizontal: number,
  vertical: number,
) {
  model.rotateOnWorldAxis(DRAG_X_AXIS, horizontal);
  model.rotateOnWorldAxis(DRAG_Y_AXIS, vertical);
}

function frameModel(
  source: THREE.Group,
  camera: THREE.PerspectiveCamera,
  distanceScale: number,
) {
  const model = new THREE.Group();
  model.add(source);
  const bounds = new THREE.Box3().setFromObject(source);
  const center = bounds.getCenter(new THREE.Vector3());
  const radius = bounds.getBoundingSphere(new THREE.Sphere()).radius;
  model.position.copy(center).multiplyScalar(-1);
  camera.position.z =
    (radius / Math.tan(THREE.MathUtils.degToRad(camera.fov / 2))) *
    distanceScale;
  return model;
}

function color(name: string) {
  return getComputedStyle(document.documentElement)
    .getPropertyValue(name)
    .trim();
}

export function boundedPixelRatio(pixelRatio = devicePixelRatio) {
  return Math.min(pixelRatio, 1);
}

function disposeScene(root: THREE.Object3D) {
  const textures = new Set<THREE.Texture>();
  const materials = new Set<THREE.Material>();
  const geometries = new Set<THREE.BufferGeometry>();

  root.traverse((object) => {
    if (!(object instanceof THREE.Mesh)) return;
    geometries.add(object.geometry);
    const meshMaterials = Array.isArray(object.material)
      ? object.material
      : [object.material];
    for (const material of meshMaterials) {
      materials.add(material);
      for (const value of Object.values(material)) {
        if (value instanceof THREE.Texture) textures.add(value);
      }
    }
  });

  for (const texture of textures) texture.dispose();
  for (const material of materials) material.dispose();
  for (const geometry of geometries) geometry.dispose();
}

function assert(condition: unknown, message: string): asserts condition {
  if (!condition) throw new Error(message);
}
