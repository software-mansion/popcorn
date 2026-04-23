#!/bin/bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
JS_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"
PROJECT_ROOT="$(cd "${JS_ROOT}/../.." && pwd)"
OUT_DIR="${PROJECT_ROOT}/otp/out"
ASSETS_DIR="${JS_ROOT}/assets"
DIST_ASSETS_DIR="${JS_ROOT}/dist/assets"
MODE="${1:-setup}"

have_runtime_artifacts() {
  local path

  for path in beam.wasm beam.smp beam.emu; do
    if [[ ! -f "${OUT_DIR}/${path}" ]]; then
      return 1
    fi
  done

  return 0
}

ensure_runtime_artifacts() {
  if have_runtime_artifacts; then
    echo "otp/js: using existing OTP WASM artifacts from ${OUT_DIR}"
    return
  fi

  echo "otp/js: building OTP WASM artifacts..."
  "${PROJECT_ROOT}/scripts/build-beam.sh" debug
}

copy_runtime_assets() {
  mkdir -p "${ASSETS_DIR}" "${ASSETS_DIR}/bin" "${ASSETS_DIR}/lib"

  for path in beam.wasm beam.smp beam.emu; do
    if [[ ! -f "${OUT_DIR}/${path}" ]]; then
      echo "otp/js: missing expected artifact '${OUT_DIR}/${path}'" >&2
      exit 1
    fi
  done

  cp "${OUT_DIR}/beam.wasm" "${ASSETS_DIR}/beam.wasm"
  cp "${OUT_DIR}/beam.smp" "${ASSETS_DIR}/beam.mjs"
  cp "${OUT_DIR}/beam.emu" "${ASSETS_DIR}/beam.emu.mjs"
  cp -R "${OUT_DIR}/bin/." "${ASSETS_DIR}/bin/"
  cp -R "${OUT_DIR}/lib/." "${ASSETS_DIR}/lib/"

  ASSETS_DIR="${ASSETS_DIR}" node <<'EOF'
const { readFileSync, writeFileSync } = require("node:fs");
const { join } = require("node:path");

const assetsDir = process.env.ASSETS_DIR;
if (assetsDir === undefined) {
  throw new Error("ASSETS_DIR is required");
}

for (const filename of ["beam.mjs", "beam.emu.mjs"]) {
  const path = join(assetsDir, filename);
  const source = readFileSync(path, "utf8");
  const patchedSource = source.replaceAll('"beam.emu"', '"beam.emu.mjs"');

  if (source !== patchedSource) {
    writeFileSync(path, patchedSource);
  }
}
EOF
}

copy_dist_assets() {
  mkdir -p "${DIST_ASSETS_DIR}" "${DIST_ASSETS_DIR}/bin" "${DIST_ASSETS_DIR}/lib"

  cp "${ASSETS_DIR}/beam.mjs" "${DIST_ASSETS_DIR}/beam.mjs"
  cp "${ASSETS_DIR}/beam.emu.mjs" "${DIST_ASSETS_DIR}/beam.emu.mjs"
  cp "${ASSETS_DIR}/beam.wasm" "${DIST_ASSETS_DIR}/beam.wasm"
  cp -R "${ASSETS_DIR}/bin/." "${DIST_ASSETS_DIR}/bin/"
  cp -R "${ASSETS_DIR}/lib/." "${DIST_ASSETS_DIR}/lib/"
}

case "${MODE}" in
  setup)
    ;;
  dist)
    copy_dist_assets
    exit 0
    ;;
  *)
    echo "Usage: $0 [setup|dist]" >&2
    exit 1
    ;;
esac

ensure_runtime_artifacts
copy_runtime_assets
