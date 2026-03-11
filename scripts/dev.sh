#!/bin/bash
set -e

LOG_PREFIX="DEV"
# shellcheck source=_common.sh
source "$(dirname "${BASH_SOURCE[0]}")/_common.sh"

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

unknown_example() {
    echo -e "${RED}${LOG_PREFIX}: ERROR |${NC} $1" >&2
    echo ""
    echo "Available examples:"
    list_examples
    exit 1
}

unknown_project() {
    echo -e "${RED}${LOG_PREFIX}: ERROR |${NC} $1" >&2
    echo ""
    echo "Available projects:"
    list_projects
    exit 1
}

require_cmd() {
    command -v "$1" &> /dev/null || error "$1 is required but not found"
}

ensure_wasm_assets() {
    local js_dir="$1"
    if [[ ! -f "${js_dir}/assets/AtomVM.wasm" ]]; then
        log "WASM assets not found, building..."
        cd "${js_dir}"
        pnpm run assets:dev
    fi
}

ensure_js_dist() {
    local js_dir="$1"
    if [[ ! -f "${js_dir}/dist/index.mjs" ]]; then
        log "JS dist not found, building..."
        cd "${js_dir}"
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

main() {
    local EXAMPLE=""
    local PROJECT=""

    while [[ $# -gt 0 ]]; do
        case "$1" in
            -h|--help) usage ;;
            --example)
                if [[ -z "${2:-}" || "$2" == --* ]]; then
                    unknown_example "--example requires a name."
                fi
                EXAMPLE="$2"
                if [[ ! -d "${PROJECT_ROOT}/examples/${EXAMPLE}" ]]; then
                    unknown_example "Unknown example '${EXAMPLE}'."
                fi
                shift 2
                ;;
            --project)
                if [[ -z "${2:-}" || "$2" == --* ]]; then
                    unknown_project "--project requires a name."
                fi
                PROJECT="$2"
                if [[ ! -d "${PROJECT_ROOT}/${PROJECT}" ]]; then
                    unknown_project "Unknown project '${PROJECT}'."
                fi
                shift 2
                ;;
            *) error "Unknown option: $1" ;;
        esac
    done

    local js_dir="${PROJECT_ROOT}/popcorn/js"
    WATCHER_CMDS=()

    load_env
    require_cmd pnpm

    log "Installing dependencies..."
    cd "${PROJECT_ROOT}"
    pnpm install

    cd "${PROJECT_ROOT}/popcorn/elixir"
    mix deps.get

    if [[ -n "${PROJECT}" ]]; then
        local project_dir="${PROJECT_ROOT}/${PROJECT}"

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
            *) unknown_project "Unknown project '${PROJECT}'." ;;
        esac
    elif [[ "${EXAMPLE}" == "eval-in-wasm" ]]; then
         local example_dir="${PROJECT_ROOT}/examples/${EXAMPLE}"
         log "Setting up example: ${EXAMPLE}"
         cd "${example_dir}"
         mix dev

    elif [[ "${EXAMPLE}" == "game-of-life" ]]; then
         local example_dir="${PROJECT_ROOT}/examples/${EXAMPLE}"
         log "Setting up example: ${EXAMPLE}"
         cd "${example_dir}"
         mix dev

    elif [[ "${EXAMPLE}" == "hello-popcorn" ]]; then
         local example_dir="${PROJECT_ROOT}/examples/${EXAMPLE}"
         log "Setting up example: ${EXAMPLE}"
         cd "${example_dir}"
         mix dev

    elif [[ -n "${EXAMPLE}" ]]; then
        ensure_wasm_assets "${js_dir}"
        ensure_js_dist "${js_dir}"

        local example_dir="${PROJECT_ROOT}/examples/${EXAMPLE}"

        log "Setting up example: ${EXAMPLE}"
        cd "${example_dir}"
        mix deps.get

        log "Cooking example..."
        mix popcorn.cook

        if [[ -n "${ATOMVM_SOURCE:-}" && -d "${ATOMVM_SOURCE}" ]]; then
            watch dir="${ATOMVM_SOURCE}/src" exts="c,h,cpp" cmd="cd '${js_dir}' && pnpm run assets:dev && pnpm --filter @swmansion/popcorn exec rollup -c"
        fi
        watch dir="${js_dir}/src" exts="ts,js" cmd="cd '${js_dir}' && pnpm --filter @swmansion/popcorn exec rollup -c"
        watch dir="${example_dir}/lib" exts="ex" cmd="cd '${example_dir}' && mix popcorn.cook"
        start_watchers

        success "Starting example server..."
        elixir --erl "+Bi" -S mix popcorn.server &
        local SERVER_PID=$!
        trap "kill ${SERVER_PID} 2>/dev/null; exit 0" INT TERM
        wait ${SERVER_PID}
    else
        ensure_wasm_assets "${js_dir}"
        success "Starting JS library in watch mode..."
        cd "${js_dir}"
        pnpm run dev
    fi
}

main "$@"
