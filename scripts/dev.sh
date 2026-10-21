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

    load_env
    require_cmd pnpm

    install_pnpm_workspace_deps "${PROJECT_ROOT}" "pnpm workspace deps"
    install_elixir_deps "${PROJECT_ROOT}/popcorn/elixir" "Popcorn Elixir deps"
    pnpm --dir "${js_dir}" build

    if [[ -n "${PROJECT}" ]]; then
        local project_dir="${PROJECT_ROOT}/${PROJECT}"

        log "Setting up project: ${PROJECT}"
        cd "${project_dir}"

        case "${PROJECT}" in
            landing-page)
                cd "${project_dir}"
                install_pnpm_deps "${project_dir}" "${PROJECT} deps"
                success "Starting landing page dev server..."
                pnpm run dev
                ;;
            language-tour)
                install_elixir_deps "${project_dir}/elixir_tour" "language-tour Elixir deps"
                install_pnpm_deps "${project_dir}" "${PROJECT} deps"
                cd "${project_dir}"
                success "Starting language tour dev server..."
                pnpm run dev
                ;;
            *) unknown_project "Unknown project '${PROJECT}'." ;;
        esac
    elif [[ -n "${EXAMPLE}" ]]; then
        local example_dir="${PROJECT_ROOT}/examples/${EXAMPLE}"
        log "Setting up example: ${EXAMPLE}"
        cd "${example_dir}"

        case "${EXAMPLE}" in
            hello-react)
                install_elixir_deps "${example_dir}/ex_app" "${EXAMPLE} Elixir deps"
                pnpm run dev
                ;;
            eval-in-wasm|game-of-life|hello-popcorn|iex-wasm|local-lv-burrito|local-lv-compare|local-lv-forms|local-lv-thermostat|local-lv-pong)
                install_elixir_deps "${example_dir}" "${EXAMPLE} Elixir deps"
                mix dev
                ;;
            *) error "Use examples/${EXAMPLE}/README.md for this example's development commands" ;;
        esac
    else
        success "Starting JS library in watch mode..."
        cd "${js_dir}"
        pnpm exec rollup -c --watch
    fi
}

main "$@"
