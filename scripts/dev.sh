#!/bin/bash
set -e

LOG_PREFIX="DEV"
# shellcheck source=_common.sh
source "$(dirname "${BASH_SOURCE[0]}")/_common.sh"

EXAMPLE=""
PROJECT=""

usage() {
    cat << EOF
Usage: $0 [OPTIONS]

Start development mode with file watching.

Options:
  --example <name>    Watch and serve an example (e.g., hello-popcorn)
  --project <name>    Watch and serve a project (e.g., landing-page)
  -h, --help          Show this help

Available examples:
$(list_examples)

Available projects:
$(list_projects)
EOF
    exit 0
}

while [[ $# -gt 0 ]]; do
    case "$1" in
        -h|--help) usage ;;
        --example)
            EXAMPLE="$2"
            shift 2
            ;;
        --project)
            PROJECT="$2"
            shift 2
            ;;
        *) error "Unknown option: $1" ;;
    esac
done

JS_DIR="${PROJECT_ROOT}/popcorn/js"
WATCHER_CMDS=()

require_cmd() {
    command -v "$1" &> /dev/null || error "$1 is required but not found"
}

ensure_wasm_assets() {
    if [[ ! -f "${JS_DIR}/assets/AtomVM.wasm" ]]; then
        log "WASM assets not found, building..."
        cd "${JS_DIR}"
        pnpm run assets:dev
    fi
}

ensure_js_dist() {
    if [[ ! -f "${JS_DIR}/dist/index.mjs" ]]; then
        log "JS dist not found, building..."
        cd "${JS_DIR}"
        pnpm --filter @swmansion/popcorn exec rollup -c
    fi
}

# Queue a file watcher: watch dir=<path> exts=<extensions> cmd=<command>
watch() {
    local dir="" exts="" cmd=""
    for arg in "$@"; do
        case "${arg}" in
            dir=*) dir="${arg#dir=}" ;;
            exts=*) exts="${arg#exts=}" ;;
            cmd=*) cmd="${arg#cmd=}" ;;
            *) error "watch: unknown arg '${arg}'" ;;
        esac
    done
    [[ -n "${dir}" ]] || error "watch: dir= is required"
    [[ -n "${exts}" ]] || error "watch: exts= is required"
    [[ -n "${cmd}" ]] || error "watch: cmd= is required"
    WATCHER_CMDS+=("${dir}" "${exts}" "${cmd}")
}

# Launch all queued watchers in a monitor subshell
start_watchers() {
    if [[ ${#WATCHER_CMDS[@]} -eq 0 ]]; then
        return
    fi

    if ! command -v watchexec &> /dev/null; then
        warn "watchexec not found, install it for auto-rebuild on file changes (mise install)"
        return
    fi

    local main_pid=$$
    local dirs=()
    for ((i = 0; i < ${#WATCHER_CMDS[@]}; i += 3)); do
        dirs+=("${WATCHER_CMDS[i]#"${PROJECT_ROOT}"/}")
    done

    (
        for ((i = 0; i < ${#WATCHER_CMDS[@]}; i += 3)); do
            local dir="${WATCHER_CMDS[i]}"
            local exts="${WATCHER_CMDS[i+1]}"
            local cmd="${WATCHER_CMDS[i+2]}"
            watchexec --postpone -w "${dir}" -e "${exts}" -- \
                bash -c "${cmd} || kill ${main_pid}" &
        done
        while kill -0 ${main_pid} 2>/dev/null; do sleep 5; done
        kill 0 2>/dev/null
    ) &

    trap 'error "A watched command failed"' TERM
    log "Watching for changes (${dirs[*]})"
}

load_env
require_cmd pnpm

log "Installing dependencies..."
cd "${PROJECT_ROOT}"
pnpm install

cd "${PROJECT_ROOT}/popcorn/elixir"
mix deps.get

if [[ -n "${PROJECT}" ]]; then
    project_dir="${PROJECT_ROOT}/${PROJECT}"

    if [[ ! -d "${project_dir}" ]]; then
        error "Project '${PROJECT}' not found at ${project_dir}"
    fi

    log "Setting up project: ${PROJECT}"
    cd "${project_dir}"

    case "${PROJECT}" in
        landing-page)
            cd "${project_dir}"
            pnpm install
            success "Starting landing page dev server..."
            pnpm run dev
            ;;
        language-tour)
            cd "${project_dir}/elixir_tour"
            mix deps.get
            cd "${project_dir}"
            pnpm install
            success "Starting language tour dev server..."
            pnpm run dev
            ;;
        *) error "Unknown project '${PROJECT}'. Use --help to see available projects." ;;
    esac
elif [[ -n "${EXAMPLE}" ]]; then
    ensure_wasm_assets
    ensure_js_dist

    example_dir="${PROJECT_ROOT}/examples/${EXAMPLE}"

    if [[ ! -d "${example_dir}" ]]; then
        error "Example '${EXAMPLE}' not found at ${example_dir}"
    fi

    log "Setting up example: ${EXAMPLE}"
    cd "${example_dir}"
    mix deps.get

    log "Building example..."
    mix build_wasm

    if [[ -n "${ATOMVM_SOURCE:-}" && -d "${ATOMVM_SOURCE}" ]]; then
        watch dir="${ATOMVM_SOURCE}/src" exts="c,h,cpp" cmd="cd '${JS_DIR}' && pnpm run assets:dev && pnpm --filter @swmansion/popcorn exec rollup -c"
    fi
    watch dir="${JS_DIR}/src" exts="ts,js" cmd="cd '${JS_DIR}' && pnpm --filter @swmansion/popcorn exec rollup -c"
    watch dir="${mix_dir}/lib" exts="ex" cmd="cd '${mix_dir}' && mix build_assets"
    start_watchers

    success "Starting example server..."
    elixir --erl "+Bi" -S mix popcorn.server &
    SERVER_PID=$!
    trap "kill ${SERVER_PID} 2>/dev/null; exit 0" INT TERM
    wait ${SERVER_PID}
else
    ensure_wasm_assets
    success "Starting JS library in watch mode..."
    cd "${JS_DIR}"
    pnpm run dev
fi
