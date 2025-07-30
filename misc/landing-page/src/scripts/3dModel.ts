import * as THREE from "three";
import { GLTFLoader } from "three/addons/loaders/GLTFLoader.js";
import type { GLTF } from "three/addons/loaders/GLTFLoader.js";
import { DRACOLoader } from "three/addons/loaders/DRACOLoader.js";

/**
 * `x` and `y` takes values in range of [-1, 1]. Default is 0 (centered).
 */
type Position = {
  x: number;
  y: number;
};

/**
 * When model position is in the middle of the section, it should have specified `position` and `scale`.
 * It should interpolate between sections.
 */
type SectionPosition = {
  position: Position;
  scale: number;
};

type Config = {
  path: string;
  containerId: string;
  sectionConfig: SectionConfigRaw;
};

type SectionConfigRaw = Record<string, SectionPosition>;
type SectionConfig = Map<string, SectionPosition>;

type State = {
  mouse: {
    isDragging: boolean;
    lastMousePosition: { x: number; y: number };
  };
  rotationVelocity: { x: number; y: number };

  scale: number;
  targetScale: number;

  position: Position;
  targetPosition: Position;
};

type RotateCallback = (deltaX: number, deltaY: number, decay: number) => void;

const DRAGGING_COEF = 0.002;
const DECAY_COEF = 0.02;
const NO_DECAY_COEF = 0;
const AUTO_ROTATION_COEF = 0.2;
// lower is slower
const LERP_COEF = 0.02;
// normalize to world positions
const POSITION_COEF = 10;
const DEFAULT_SECTION_PARAMS = {
  scale: 1,
  position: { x: 0, y: 0 },
};

export async function initModel(config: Config) {
  const sectionConfig = new Map(Object.entries(config.sectionConfig));
  validateSectionPositions(sectionConfig);

  const firstSection = firstMapValue(sectionConfig) ?? DEFAULT_SECTION_PARAMS;
  const state: State = {
    rotationVelocity: { x: 0, y: 0 },
    mouse: {
      isDragging: false,
      lastMousePosition: { x: 0, y: 0 },
    },
    scale: firstSection.scale,
    position: firstSection.position,
    targetScale: firstSection.scale,
    targetPosition: firstSection.position,
  };

  const loader = createLoader();
  const gltf = await loadModel(loader, config.path);
  const model = gltf.scene;

  const mixer = new THREE.AnimationMixer(model);
  const clock = new THREE.Clock();
  const scene = new THREE.Scene();
  const camera = createCamera();

  for (const clip of gltf.animations) {
    runAnimation(clip, mixer, 0.3);
  }

  const RGB_WHITE = 0xffffff;
  const ambientLight = new THREE.AmbientLight(RGB_WHITE, 1.2);
  const directionalLight = new THREE.DirectionalLight(RGB_WHITE, 1.5);
  for (const object of [ambientLight, directionalLight, model]) {
    scene.add(object);
  }

  const renderer = createAndMountRenderer(config.containerId, camera);

  const updateRotation = createRotationCallback(model, state);
  setupDragging(updateRotation, state);
  setupSectionObserver(sectionConfig, state);

  function animate() {
    const delta = clock.getDelta();
    const t = clock.getElapsedTime();
    const dragging = state.mouse.isDragging;

    // slow rotation to upper left
    const autoRotationX = AUTO_ROTATION_COEF * delta;
    const autoRotationY = -AUTO_ROTATION_COEF * delta;
    updateRotation(autoRotationX, autoRotationY, NO_DECAY_COEF);

    // rotation velocity on mouse interaction
    updateRotation(
      state.rotationVelocity.x,
      state.rotationVelocity.y,
      dragging ? NO_DECAY_COEF : DECAY_COEF,
    );

    // interpolate position and scale
    const { scale, targetScale } = state;
    state.scale += (targetScale - scale) * LERP_COEF;

    const { x, y } = state.position;
    const targetX = state.targetPosition.x;
    const targetY = state.targetPosition.y;
    state.position.x += (targetX - x) * LERP_COEF;
    state.position.y += (targetY - y) * LERP_COEF;

    // apply position
    model.position.x = state.position.x * POSITION_COEF;
    model.position.y = state.position.y * POSITION_COEF;

    // pulse animation
    model.scale.setScalar(state.scale * pulse(t));

    mixer.update(delta);
    renderer.render(scene, camera);
  }
  renderer.setAnimationLoop(animate);
}

function pulse(t: number) {
  const SPEED = 1.5;
  const DIFF = 0.05;
  return 1 + Math.sin(t * SPEED) * DIFF;
}

function createRotationCallback(
  model: THREE.Object3D,
  state: State,
): RotateCallback {
  return (deltaX: number, deltaY: number, decay: number) => {
    model.rotation.y += deltaX;
    model.rotation.x += deltaY;

    state.rotationVelocity.x *= 1 - decay;
    state.rotationVelocity.y *= 1 - decay;
  };
}

function runAnimation(
  clip: THREE.AnimationClip,
  mixer: THREE.AnimationMixer,
  timeScale: number,
) {
  const action = mixer.clipAction(clip);
  action.timeScale = timeScale;
  action.play();
}

function createCamera() {
  const camera = new THREE.PerspectiveCamera();
  camera.position.set(0, 0, 15);
  return camera;
}

function createAndMountRenderer(id: string, camera: THREE.PerspectiveCamera) {
  const renderer = new THREE.WebGLRenderer({
    antialias: true,
    alpha: true,
  });

  const container = document.getElementById(id);
  if (container === null) {
    throw new Error(`Element '${id}' not found`);
  }

  resizeRenderer(container, camera, renderer);
  window.addEventListener("resize", () =>
    resizeRenderer(container, camera, renderer),
  );
  container.appendChild(renderer.domElement);

  return renderer;
}

function resizeRenderer(
  container: HTMLElement,
  camera: THREE.PerspectiveCamera,
  renderer: THREE.WebGLRenderer,
) {
  renderer.setSize(container.clientWidth, container.clientHeight);
  camera.aspect = container.clientWidth / container.clientHeight;
  camera.updateProjectionMatrix();
}

function createLoader() {
  const dracoLoader = new DRACOLoader();
  dracoLoader.setDecoderPath(
    "https://www.gstatic.com/draco/versioned/decoders/1.5.6/",
  );

  const loader = new GLTFLoader();
  loader.setDRACOLoader(dracoLoader);
  return loader;
}

function loadModel(loader: GLTFLoader, modelPath: string) {
  return new Promise<GLTF>((resolve, reject) => {
    loader.load(modelPath, resolve, undefined, reject);
  });
}

function setupDragging(onRotate: RotateCallback, state: State) {
  function start(event: MouseEvent | TouchEvent) {
    const x = isTouchEvent(event) ? event.touches[0].clientX : event.clientX;
    const y = isTouchEvent(event) ? event.touches[0].clientY : event.clientY;

    state.mouse.isDragging = true;
    state.mouse.lastMousePosition.x = x;
    state.mouse.lastMousePosition.y = y;
  }

  function update(event: MouseEvent | TouchEvent) {
    if (!state.mouse.isDragging) {
      return;
    }

    const x = isTouchEvent(event) ? event.touches[0].clientX : event.clientX;
    const y = isTouchEvent(event) ? event.touches[0].clientY : event.clientY;
    const lastX = state.mouse.lastMousePosition.x;
    const lastY = state.mouse.lastMousePosition.y;

    const deltaX = (x - lastX) * DRAGGING_COEF;
    const deltaY = (y - lastY) * DRAGGING_COEF;

    onRotate(deltaX, deltaY, NO_DECAY_COEF);

    state.rotationVelocity.x = deltaX;
    state.rotationVelocity.y = deltaY;
    state.mouse.lastMousePosition.x = x;
    state.mouse.lastMousePosition.y = y;
  }

  function end(_event: MouseEvent | TouchEvent) {
    state.mouse.isDragging = false;
  }

  window.addEventListener("mousedown", start);
  window.addEventListener("touchstart", start);

  window.addEventListener("mousemove", update);
  window.addEventListener("touchmove", update);

  window.addEventListener("mouseup", end);
  window.addEventListener("mouseleave", end);
  window.addEventListener("touchend", end);
  window.addEventListener("touchcancel", end);
}

function validateSectionPositions(sectionConfig: SectionConfig) {
  const sectionIds = Array.from(sectionConfig.keys());
  const selectors = sectionIds.map((id) => `#${id}`).join(", ");

  const n = sectionIds.length;
  const foundN = document.querySelectorAll(selectors).length;
  if (n !== foundN) {
    throw new Error("Some sections missing");
  }
}

function setupSectionObserver(sectionConfig: SectionConfig, state: State) {
  function onIntersect(entries: IntersectionObserverEntry[]) {
    if (entries.length === 0) {
      return;
    }

    const mostVisibleEntry = maxBy(entries, (e) => e.intersectionRatio)!;
    if (!mostVisibleEntry.isIntersecting) {
      return;
    }

    const config = sectionConfig.get(mostVisibleEntry.target.id);
    if (config === undefined) {
      return;
    }

    state.targetPosition = config.position;
    state.targetScale = config.scale;
  }

  const observer = new IntersectionObserver(onIntersect, {
    threshold: [0.3],
  });

  sectionConfig
    .keys()
    .map((sectionId) => document.getElementById(sectionId)!)
    .forEach((element) => observer.observe(element));
}

function isTouchEvent(event: MouseEvent | TouchEvent): event is TouchEvent {
  return event.type.startsWith("touch");
}

function firstMapValue<T>(map: Map<unknown, T>): T | null {
  return map.values().next().value ?? null;
}

function maxBy<T>(list: T[], getAttr: (element: T) => number): T | null {
  if (list.length === 0) {
    return null;
  }

  let max = list[0];
  for (const element of list) {
    if (getAttr(element) > getAttr(max)) {
      max = element;
    }
  }
  return max;
}
