import * as THREE from "three";
import { GLTFLoader } from "three/addons/loaders/GLTFLoader.js";
import type { GLTF } from "three/addons/loaders/GLTFLoader.js";
import { DRACOLoader } from "three/addons/loaders/DRACOLoader.js";

type ModelState = {
  mouse: THREE.Vector2;
  previousMouse: THREE.Vector2;
  isDragging: boolean;
  rotationVelocity: { x: number; y: number };
  scrollY: number;
  time: number;
  modelScale: number;
  baseScale: number;
  originalScale: THREE.Vector3;
};

type ScrollStage = {
  sectionId: string;
  position: { x: number; y: number };
  scale: number;
  offset?: number; // offset from section start (0-1)
};

type ModelOptions = {
  scrollStages?: ScrollStage[];
};

export async function initModel(
  path: string,
  containerId: string,
  options: ModelOptions = {},
) {
  const loader = createLoader();
  const gltf = await loadModel(loader, path);
  const model = gltf.scene;
  const mixer = new THREE.AnimationMixer(model);

  // Use first scroll stage position as initial position if available
  const initialPosition = options.scrollStages?.[0]?.position;
  const state = createState(model, initialPosition);

  const camera = createCamera();
  const scene = new THREE.Scene();
  const lights = createLights();
  for (const light of lights) {
    scene.add(light);
  }

  const renderer = createAndMountRenderer(containerId);
  setupAnimations(gltf, mixer);

  const { onPointerDown, onPointerUp, onPointerMove } = createPointerHandlers(
    renderer,
    model,
    state,
  );
  const onResize = createResizeHandler(camera, renderer);

  if (options.scrollStages) {
    const onScroll = createScrollHandler(state);
    window.addEventListener("scroll", onScroll, { passive: true });
  }
  window.addEventListener("resize", onResize);

  window.addEventListener("pointerdown", onPointerDown);
  window.addEventListener("pointermove", onPointerMove);
  window.addEventListener("pointerup", onPointerUp);

  scene.add(model);
  startRenderLoop(scene, camera, renderer, model, state, mixer, options);
}

function createCamera() {
  const camera = new THREE.PerspectiveCamera(75, 1, 0.1, 1000);
  camera.position.set(0, 0, 5);
  camera.lookAt(0, 0, 0);
  return camera;
}

function createAndMountRenderer(id: string) {
  const renderer = new THREE.WebGLRenderer({
    antialias: true,
    alpha: true,
  });

  const container = document.getElementById(id);
  if (container === null) {
    throw new Error(`Element '${id}' not found`);
  }

  renderer.setSize(window.innerWidth, window.innerHeight);
  renderer.setClearColor(0x000000, 0);
  renderer.domElement.style.pointerEvents = "auto";
  container.appendChild(renderer.domElement);

  return renderer;
}

function createLights() {
  const ambientLight = new THREE.AmbientLight(0xffffff, 1.2);

  const directionalLight1 = new THREE.DirectionalLight(0xffffff, 1.5);
  directionalLight1.position.set(5, 5, 5);

  const directionalLight2 = new THREE.DirectionalLight(0xffffff, 0.8);
  directionalLight2.position.set(-5, -5, 2);

  const pointLight = new THREE.PointLight(0xffffff, 1.0, 100);
  pointLight.position.set(0, 0, 8);

  return [ambientLight, directionalLight1, directionalLight2, pointLight];
}

function createLoader() {
  const dracoLoader = new DRACOLoader();
  // TODO: use vendored assets
  dracoLoader.setDecoderPath(
    "https://www.gstatic.com/draco/versioned/decoders/1.5.6/",
  );

  const loader = new GLTFLoader();
  loader.setDRACOLoader(dracoLoader);
  return loader;
}

function createState(
  model: THREE.Group,
  initialPosition?: { x: number; y: number },
): ModelState {
  const startX = initialPosition?.x ?? 0;
  const startY = initialPosition?.y ?? 0;
  model.position.set(startX, startY, 0);

  const box = new THREE.Box3().setFromObject(model);
  const size = box.getSize(new THREE.Vector3());
  const maxDim = Math.max(size.x, size.y, size.z);
  const targetScale = maxDim > 2 ? 2 / maxDim : 1;

  const originalScale = new THREE.Vector3();
  originalScale.copy(model.scale);
  model.scale.set(0, 0, 0);

  return {
    mouse: new THREE.Vector2(),
    previousMouse: new THREE.Vector2(),
    isDragging: false,
    rotationVelocity: { x: 0, y: 0 },

    scrollY: window.pageYOffset || document.documentElement.scrollTop,
    time: 0,
    modelScale: 0,
    baseScale: targetScale,
    originalScale,
  };
}

function loadModel(loader: GLTFLoader, modelPath: string) {
  return new Promise<GLTF>((resolve, reject) => {
    loader.load(modelPath, resolve, undefined, reject);
  });
}

function setupAnimations(gltf: GLTF, mixer: THREE.AnimationMixer) {
  for (const clip of gltf.animations) {
    const action = mixer.clipAction(clip);
    action.timeScale = 0.3;
    action.play();
  }
}

function createPointerHandlers(
  renderer: THREE.WebGLRenderer,
  model: THREE.Object3D,
  state: ModelState,
) {
  function onPointerDown(event: PointerEvent) {
    state.isDragging = true;

    updateMousePosition(event, renderer, state.mouse);
    state.previousMouse.copy(state.mouse);
  }

  function onPointerMove(event: PointerEvent) {
    if (!state.isDragging) return;

    const previousMouse = state.previousMouse.clone();
    updateMousePosition(event, renderer, state.mouse);

    const deltaX = state.mouse.x - previousMouse.x;
    const deltaY = state.mouse.y - previousMouse.y;

    applyRotation(model, deltaX, deltaY, state.rotationVelocity);
    state.previousMouse.copy(state.mouse);
  }

  function onPointerUp() {
    state.isDragging = false;
  }

  return { onPointerDown, onPointerUp, onPointerMove };
}

function createScrollHandler(state: ModelState) {
  return () => {
    state.scrollY = window.pageYOffset || document.documentElement.scrollTop;
  };
}

function createResizeHandler(
  camera: THREE.PerspectiveCamera,
  renderer: THREE.WebGLRenderer,
) {
  return () => {
    camera.aspect = window.innerWidth / window.innerHeight;
    camera.updateProjectionMatrix();
    renderer.setSize(window.innerWidth, window.innerHeight);
  };
}

function updateMousePosition(
  event: PointerEvent,
  renderer: THREE.WebGLRenderer,
  mouse: THREE.Vector2,
) {
  const rect = renderer.domElement.getBoundingClientRect();
  mouse.x = ((event.clientX - rect.left) / rect.width) * 2 - 1;
  mouse.y = -((event.clientY - rect.top) / rect.height) * 2 + 1;
}

function applyRotation(
  model: THREE.Object3D,
  deltaX: number,
  deltaY: number,
  rotationVelocity: { x: number; y: number },
) {
  const rotationScale = 2;
  model.rotation.y += deltaX * rotationScale;
  model.rotation.x += deltaY * rotationScale;

  rotationVelocity.x = deltaY * rotationScale * 0.5;
  rotationVelocity.y = deltaX * rotationScale * 0.5;
}

function applyScrollFollowing(model: THREE.Object3D, scrollY: number) {
  const scrollOffset = scrollY * 0.0005;
  model.position.y = -scrollOffset;
  model.position.x = 2;
}

function getSectionScrollPosition(
  sectionId: string,
  offset: number = 0,
): number {
  const section = document.getElementById(sectionId);
  if (section === null) {
    throw new Error(`Section with id "${sectionId}" not found`);
  }

  const sectionTop = section.offsetTop;
  const sectionHeight = section.offsetHeight;

  return sectionTop + sectionHeight * offset;
}

type TransformedScrollStage = {
  position: ScrollStage["position"];
  scale: ScrollStage["scale"];
  scrollY: number;
};
function applyScrollStages(
  model: THREE.Object3D,
  state: ModelState,
  stages: ScrollStage[],
) {
  if (stages.length === 0) {
    return;
  }

  const scrollStages: TransformedScrollStage[] = [];
  for (const stage of stages) {
    const scrollY = getSectionScrollPosition(
      stage.sectionId,
      stage.offset ?? 0,
    );
    const position = stage.position;
    const scale = stage.scale;

    const forwardScroll = scrollY >= 0;
    if (forwardScroll) {
      scrollStages.push({ scrollY, position, scale });
    }
  }
  scrollStages.sort((a, b) => a.scrollY - b.scrollY);

  const noForwardStages = scrollStages.length === 0;
  if (noForwardStages) {
    return;
  }

  const currentScrollY = state.scrollY;
  const nextStageIdx = scrollStages.findIndex(
    ({ scrollY }, _i, _list) => scrollY >= currentScrollY,
  );
  if (nextStageIdx <= 0) {
    return;
  }
  const currentStage = scrollStages[nextStageIdx - 1];
  const nextStage = scrollStages[nextStageIdx];

  const stagesYDiff = nextStage.scrollY - currentStage.scrollY;
  const currentYDiff = (state.scrollY - currentStage.scrollY) / stagesYDiff;
  const t = THREE.MathUtils.clamp(currentYDiff, 0, 1);

  const x = currentStage.position.x;
  const nextX = nextStage.position.x;
  model.position.x = THREE.MathUtils.lerp(x, nextX, t);

  const y = currentStage.position.y;
  const nextY = nextStage.position.y;
  model.position.y = THREE.MathUtils.lerp(y, nextY, t);

  const currentScale = currentStage.scale;
  const nextScale = nextStage.scale;
  const targetScale = THREE.MathUtils.lerp(currentScale, nextScale, t);
  state.modelScale = Math.min(state.modelScale + 0.02, 1);
  let scale = targetScale * state.baseScale * easeInCubic(state.modelScale);
  if (state.modelScale >= 1) {
    scale *= 1 + Math.sin(state.time * 2) * 0.1;
  }
  model.scale.copy(state.originalScale).multiplyScalar(scale);
}

function applyScaleAnimation(model: THREE.Object3D, state: ModelState) {
  state.modelScale = Math.min(state.modelScale + 0.02, 1);
  const fullScale = state.modelScale >= 1;

  let scale: number = null!;
  if (fullScale) {
    scale = pulseScale(state);
  } else {
    scale = easeInCubic(state.modelScale) * state.baseScale;
  }

  model.scale.copy(state.originalScale).multiplyScalar(scale);
}

function pulseScale(state: ModelState) {
  return state.baseScale * (1 + Math.sin(state.time * 2) * 0.1);
}

function applyMomentumRotation(model: THREE.Object3D, state: ModelState) {
  if (state.isDragging) return;

  model.rotation.x += state.rotationVelocity.x;
  model.rotation.y += state.rotationVelocity.y;

  state.rotationVelocity.x *= 0.98;
  state.rotationVelocity.y *= 0.98;

  const settled =
    Math.abs(state.rotationVelocity.x) < 0.001 &&
    Math.abs(state.rotationVelocity.y) < 0.001;
  if (settled) {
    model.rotation.y += 0.005;
    model.rotation.x += 0.002;
  }
}

function updateAnimations(mixer: THREE.AnimationMixer, deltaTime: number) {
  mixer.update(deltaTime);
}

function easeInCubic(t: number) {
  return t * t * t;
}

function startRenderLoop(
  scene: THREE.Scene,
  camera: THREE.PerspectiveCamera,
  renderer: THREE.WebGLRenderer,
  model: THREE.Object3D,
  state: ModelState,
  mixer: THREE.AnimationMixer,
  options: ModelOptions,
) {
  let lastTime = 0;

  function animate(currentTime: number) {
    requestAnimationFrame(animate);

    const deltaTime = (currentTime - lastTime) / 1000;
    lastTime = currentTime;

    state.time += deltaTime;

    updateAnimations(mixer, deltaTime);

    if (options.scrollStages && options.scrollStages.length > 0) {
      applyScrollStages(model, state, options.scrollStages);
    } else {
      applyScrollFollowing(model, state.scrollY);
      applyScaleAnimation(model, state);
    }

    applyMomentumRotation(model, state);

    renderer.render(scene, camera);
  }
  animate(0);
}
