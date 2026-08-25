#!/bin/bash
# Patch OTP/BEAM sources for WebAssembly (wasm32-emscripten).
set -euo pipefail

LOG_PREFIX="PATCH BEAM"
# shellcheck source=_common.sh
source "$(dirname "${BASH_SOURCE[0]}")/_common.sh"

SOURCES_DIR="${OTP_PATCH_SOURCES_DIR:-${PROJECT_ROOT}/popcorn/sources}"
OTP_DIR="${SOURCES_DIR}/otp"
OTP_ORIGINAL_DIR="${SOURCES_DIR}/otp-original"
PATCHES_DIR="${OTP_PATCHES_DIR:-${PROJECT_ROOT}/popcorn/patches}"
STAMP_FILE="${OTP_DIR}/.stamp-patched"
BASE_REF="refs/popcorn/otp-base"
PATCH_REF_PREFIX="refs/popcorn/otp-patches"
GIT_IDENTITY=(-c user.name=Popcorn -c user.email=popcorn@localhost)

usage() {
    cat << EOF
Usage:
  $0 [OPTIONS]
  $0 --regen [--create-new-patches] <revision>:<patch-name> [...]

Modes:
  (default)     Apply the ordered feature patches to popcorn/sources/otp
  --regen       Fold modification commits into feature patches and consume them

Options:
  --create-new-patches       Allow --regen to create missing feature patches
  -h, --help

Patch names contain lowercase letters separated by single hyphens, for example
"javascript-bridge". Do not include the numeric prefix or .patch suffix.

Examples:
  $0
  $0 --regen HEAD:javascript-bridge
  $0 --regen --create-new-patches HEAD:new-runtime-hook

Regeneration workflow:
  1. Commit each focused OTP modification above the applied patch stack.
  2. Assign every modification commit to exactly one simplified patch name.
  3. Use --create-new-patches only when an assignment introduces a new name.

Split a modification that belongs to several features into separate commits
before regeneration. Updating an early patch may refresh later patch files when
their diff context changes.
EOF
    exit 0
}

setup_git_excludes() {
    local exclude_file="${OTP_DIR}/.git/info/exclude"
    local marker="# otp-wasm excludes"

    if grep -q "${marker}" "${exclude_file}" 2>/dev/null; then
        return
    fi

    cat >> "${exclude_file}" << 'EXCLUDE'
# otp-wasm excludes
*.o
*.a
*.so
*.dylib
*.beam
*.wasm
/bootstrap/
/bin/
/stdlib-tarballs/
/.openssl-wasm/
**/wasm32-unknown-emscripten/
**/config.status
**/config.log
.stamp-*
**/configure
**/config.h.in
/autom4te.cache/
/lib/*/autom4te.cache/
/cross_check_erl.erl
EXCLUDE
}

valid_patch_name() {
    [[ "$1" =~ ^[a-z]+(-[a-z]+)*$ ]]
}

discover_patches() {
    local output_file="$1"
    local file base prefix name previous_number="" previous_name=""
    : > "${output_file}"

    while IFS= read -r file; do
        base=$(basename "${file}")
        if [[ ! "${base}" =~ ^([0-9]+)-([a-z]+(-[a-z]+)*)\.patch$ ]]; then
            error "Invalid feature patch filename: ${base}"
        fi
        prefix="${BASH_REMATCH[1]}"
        name="${BASH_REMATCH[2]}"
        printf '%010d\t%s\t%s\t%s\n' "$((10#${prefix}))" "${name}" "${file}" "${prefix}" >> "${output_file}"
    done < <(find "${PATCHES_DIR}" -maxdepth 1 -type f -name '*.patch' -print | sort)

    sort -n -k1,1 -o "${output_file}" "${output_file}"
    if ! awk -F '\t' '!seen[$2]++ { next } { exit 1 }' "${output_file}"; then
        error "Duplicate feature patch name."
    fi
    while IFS=$'\t' read -r number name _ prefix; do
        if [[ "${number}" == "${previous_number}" ]]; then
            error "Duplicate feature patch number: ${prefix}"
        fi
        previous_number="${number}"
        previous_name="${name}"
    done < "${output_file}"

    if [[ -z "${previous_name}" ]]; then
        error "No feature patches found in ${PATCHES_DIR}"
    fi
}

patch_hash() {
    local patches_file="$1"
    local files=()
    while IFS=$'\t' read -r _ _ file _; do
        files+=("${file}")
    done < "${patches_file}"
    cat "${files[@]}" | git hash-object --stdin
}

feature_tip() {
    local patches_file="$1"
    local tip=""
    while IFS=$'\t' read -r _ name _ _; do
        tip=$(git -C "${OTP_DIR}" rev-parse --verify "${PATCH_REF_PREFIX}/${name}^{commit}" 2>/dev/null) || return 1
    done < "${patches_file}"
    printf '%s\n' "${tip}"
}

apply_feature_patches() {
    local patches_file="$1"
    local base name file commit

    base=$(git -C "${OTP_DIR}" rev-parse HEAD)
    git -C "${OTP_DIR}" update-ref "${BASE_REF}" "${base}"

    while IFS=$'\t' read -r _ name file _; do
        log "Applying $(basename "${file}")..."
        git -C "${OTP_DIR}" apply --index "${file}"
        git -C "${OTP_DIR}" "${GIT_IDENTITY[@]}" commit --quiet \
            -m "OTP patch: ${name}" -m "Patch: ${name}"
        commit=$(git -C "${OTP_DIR}" rev-parse HEAD)
        git -C "${OTP_DIR}" update-ref "${PATCH_REF_PREFIX}/${name}" "${commit}"
    done < "${patches_file}"
}

apply_patches() {
    local patches_file patches_digest current_tip created=false
    patches_file=$(mktemp)
    discover_patches "${patches_file}"
    patches_digest=$(patch_hash "${patches_file}")

    if [[ ! -d "${OTP_ORIGINAL_DIR}" ]]; then
        error "popcorn/sources/otp-original not found. Run build-beam.sh first to clone OTP."
    fi

    if [[ ! -d "${OTP_DIR}" ]]; then
        log "Copying otp-original to otp..."
        cp -a "${OTP_ORIGINAL_DIR}" "${OTP_DIR}"
        created=true
    fi

    if [[ ! -d "${OTP_DIR}/.git" ]]; then
        error "popcorn/sources/otp is not a Git repository."
    fi

    setup_git_excludes
    git -C "${OTP_DIR}" update-index -q --refresh
    current_tip=$(feature_tip "${patches_file}" || true)
    if [[ -f "${STAMP_FILE}" ]] && [[ "$(< "${STAMP_FILE}")" == "${patches_digest}" ]] && [[ -n "${current_tip}" ]]; then
        rm -f "${patches_file}"
        log "Patches already applied and up to date, skipping."
        return
    fi

    if [[ "${created}" != "true" ]] && ! git -C "${OTP_DIR}" show-ref --verify --quiet "${BASE_REF}"; then
        rm -f "${patches_file}"
        error "Existing OTP tree predates the feature stack. Recreate it with build-beam.sh --clean."
    fi

    if git -C "${OTP_DIR}" show-ref --verify --quiet "${BASE_REF}"; then
        local base
        base=$(git -C "${OTP_DIR}" rev-parse "${BASE_REF}")
        log "Rebuilding changed patch stack from ${base}..."
        git -C "${OTP_DIR}" reset --hard --quiet "${base}"
        git -C "${OTP_DIR}" clean -fdq
        while IFS= read -r ref; do
            git -C "${OTP_DIR}" update-ref -d "${ref}"
        done < <(git -C "${OTP_DIR}" for-each-ref --format='%(refname)' "${PATCH_REF_PREFIX}/")
    fi

    apply_feature_patches "${patches_file}"
    printf '%s\n' "${patches_digest}" > "${STAMP_FILE}"
    rm -f "${patches_file}"
    success "Patches applied."
}

parse_assignments() {
    local assignments_file="$1"
    shift
    local pair revision name commit
    : > "${assignments_file}"

    if [[ $# -eq 0 ]]; then
        error "--regen requires at least one <revision>:<patch-name> assignment."
    fi

    for pair in "$@"; do
        if [[ "${pair}" != *:* ]]; then
            error "Invalid assignment '${pair}'. Expected <revision>:<patch-name>."
        fi
        revision="${pair%%:*}"
        name="${pair#*:}"
        if [[ -z "${revision}" ]] || ! valid_patch_name "${name}"; then
            error "Invalid assignment '${pair}'. Patch names must match [a-z]+(-[a-z]+)*."
        fi
        commit=$(git -C "${OTP_DIR}" rev-parse --verify "${revision}^{commit}" 2>/dev/null) || error "Unknown revision: ${revision}"
        if awk -F '\t' -v wanted="${commit}" '$1 == wanted { found=1 } END { exit found ? 0 : 1 }' "${assignments_file}"; then
            error "Revision assigned more than once: ${revision}"
        fi
        printf '%s\t%s\n' "${commit}" "${name}" >> "${assignments_file}"
    done
}

assignment_name() {
    local assignments_file="$1"
    local commit="$2"
    awk -F '\t' -v wanted="${commit}" '$1 == wanted { print $2; exit }' "${assignments_file}"
}

trailer_value() {
    git -C "${OTP_DIR}" log -1 --format="%(trailers:key=$2,valueonly)" "$1"
}

rewrite_assignment_trailers() {
    local feature_tip_commit="$1"
    local assignments_file="$2"
    local rewritten_file="$3"
    local commits_file="$4"
    local old parent tree message_file name new parent_count
    : > "${rewritten_file}"
    parent="${feature_tip_commit}"

    git -C "${OTP_DIR}" rev-list --reverse --first-parent "${feature_tip_commit}..HEAD" > "${commits_file}"
    while IFS= read -r old; do
        [[ -n "${old}" ]] || continue
        parent_count=$(git -C "${OTP_DIR}" rev-list --parents -n 1 "${old}" | wc -w | tr -d ' ')
        if [[ "${parent_count}" -ne 2 ]]; then
            error "Merge commits are not supported in the modification stack: ${old}"
        fi
        name=$(assignment_name "${assignments_file}" "${old}")
        if [[ -z "${name}" ]]; then
            local artifact
            artifact=$(trailer_value "${old}" Popcorn-Build-Artifact)
            if [[ "${artifact}" != "configure" ]] && [[ "${artifact}" != "preloaded" ]]; then
                error "Unassigned modification commit: ${old}"
            fi
        fi

        tree=$(git -C "${OTP_DIR}" rev-parse "${old}^{tree}")
        message_file=$(mktemp)
        git -C "${OTP_DIR}" log -1 --format=%B "${old}" > "${message_file}"
        if [[ -n "${name}" ]]; then
            git interpret-trailers --in-place --if-exists replace --if-missing add \
                --trailer "Patch: ${name}" "${message_file}"
        fi
        new=$(GIT_AUTHOR_NAME="$(git -C "${OTP_DIR}" log -1 --format=%an "${old}")" \
            GIT_AUTHOR_EMAIL="$(git -C "${OTP_DIR}" log -1 --format=%ae "${old}")" \
            GIT_AUTHOR_DATE="$(git -C "${OTP_DIR}" log -1 --format=%aI "${old}")" \
            GIT_COMMITTER_NAME="$(git -C "${OTP_DIR}" log -1 --format=%cn "${old}")" \
            GIT_COMMITTER_EMAIL="$(git -C "${OTP_DIR}" log -1 --format=%ce "${old}")" \
            GIT_COMMITTER_DATE="$(git -C "${OTP_DIR}" log -1 --format=%cI "${old}")" \
            git -C "${OTP_DIR}" commit-tree "${tree}" -p "${parent}" < "${message_file}")
        rm -f "${message_file}"
        if [[ -n "${name}" ]]; then
            printf '%s\t%s\n' "${new}" "${name}" >> "${rewritten_file}"
        fi
        parent="${new}"
    done < "${commits_file}"

    git -C "${OTP_DIR}" reset --hard --quiet "${parent}"
}

add_new_patch_entries() {
    local patches_file="$1"
    local assignments_file="$2"
    local allow_new="$3"
    local next_number name
    next_number=$(awk -F '\t' 'END { print $1 + 1 }' "${patches_file}")

    while IFS=$'\t' read -r _ name; do
        if awk -F '\t' -v wanted="${name}" '$2 == wanted { found=1 } END { exit found ? 0 : 1 }' "${patches_file}"; then
            continue
        fi
        if [[ "${allow_new}" != "true" ]]; then
            error "Patch does not exist: ${name}. Pass --create-new-patches to create it."
        fi
        printf '%010d\t%s\t%s/%04d-%s.patch\t%04d\n' \
            "${next_number}" "${name}" "${PATCHES_DIR}" "${next_number}" "${name}" "${next_number}" >> "${patches_file}"
        next_number=$((next_number + 1))
    done < "${assignments_file}"
}

build_regenerated_stack() {
    local repo="$1"
    local patches_file="$2"
    local assignments_file="$3"
    local output_dir="$4"
    local commits_file="$5"
    local name file commit modification assigned_name
    : > "${commits_file}"

    while IFS=$'\t' read -r _ name file _; do
        if [[ -f "${file}" ]]; then
            git -C "${repo}" apply --index "${file}"
        fi
        while IFS=$'\t' read -r modification assigned_name; do
            [[ "${assigned_name}" == "${name}" ]] || continue
            git -C "${repo}" cherry-pick --no-commit "${modification}"
        done < "${assignments_file}"
        if git -C "${repo}" diff --cached --quiet; then
            error "Patch '${name}' has no changes."
        fi
        git -C "${repo}" "${GIT_IDENTITY[@]}" commit --quiet \
            -m "OTP patch: ${name}" -m "Patch: ${name}"
        commit=$(git -C "${repo}" rev-parse HEAD)
        printf '%s\t%s\n' "${name}" "${commit}" >> "${commits_file}"
        git -C "${repo}" diff --binary --full-index "${commit}^" "${commit}" > "${output_dir}/$(basename "${file}")"
    done < "${patches_file}"
}

verify_regenerated_stack() {
    local source_repo="$1"
    local base="$2"
    local patches_file="$3"
    local output_dir="$4"
    local expected="$5"
    local verify_dir file
    verify_dir="${output_dir}.verify"
    git clone --quiet --shared --no-checkout "${source_repo}" "${verify_dir}"
    git -C "${verify_dir}" checkout --quiet --detach "${base}"
    while IFS=$'\t' read -r _ _ file _; do
        git -C "${verify_dir}" apply "${output_dir}/$(basename "${file}")"
    done < "${patches_file}"
    git -C "${verify_dir}" add -A
    git -C "${verify_dir}" diff --cached --quiet "${expected}" -- || error "Exported patch stack does not match regenerated OTP."
    rm -rf "${verify_dir}"
}

publish_patches() {
    local patches_file="$1"
    local output_dir="$2"
    local modified_file="$3"
    local created_file="$4"
    local file generated replacement
    : > "${modified_file}"
    : > "${created_file}"

    while IFS=$'\t' read -r _ _ file _; do
        generated="${output_dir}/$(basename "${file}")"
        if [[ ! -f "${file}" ]]; then
            printf '%s\n' "${file}" >> "${created_file}"
            replacement=$(mktemp "${file}.popcorn-new.XXXXXX")
            cp "${generated}" "${replacement}"
            mv "${replacement}" "${file}"
        elif ! cmp -s "${file}" "${generated}"; then
            printf '%s\n' "${file}" >> "${modified_file}"
            replacement=$(mktemp "${file}.popcorn-new.XXXXXX")
            cp "${generated}" "${replacement}"
            mv "${replacement}" "${file}"
        fi
    done < "${patches_file}"
}

print_patch_group() {
    local title="$1"
    local file="$2"
    printf '%s\n' "${title}:"
    if [[ ! -s "${file}" ]]; then
        printf '  none\n'
        return
    fi
    while IFS= read -r patch; do
        if [[ "${patch}" == "${PROJECT_ROOT}/"* ]]; then
            patch="${patch#"${PROJECT_ROOT}/"}"
        fi
        printf '  %s\n' "${patch}"
    done < "${file}"
}

regen_patches() {
    local allow_new="$1"
    shift
    local state_dir patches_file assignments_file rewritten_file commits_range_file
    local regenerated_commits_file output_dir modified_file created_file base tip regenerated_tip patches_digest

    if [[ ! -d "${OTP_DIR}/.git" ]]; then
        error "popcorn/sources/otp is not a Git repository."
    fi
    if [[ -n "$(git -C "${OTP_DIR}" status --porcelain)" ]]; then
        error "OTP has uncommitted changes. Commit them before regeneration."
    fi
    if ! git -C "${OTP_DIR}" show-ref --verify --quiet "${BASE_REF}"; then
        error "OTP feature-stack metadata is missing. Recreate it with build-beam.sh --clean."
    fi

    state_dir=$(mktemp -d)
    trap '[[ -n "${state_dir:-}" ]] && rm -rf "${state_dir}"' EXIT
    patches_file="${state_dir}/patches"
    assignments_file="${state_dir}/assignments"
    rewritten_file="${state_dir}/rewritten"
    commits_range_file="${state_dir}/range"
    regenerated_commits_file="${state_dir}/regenerated-commits"
    output_dir="${state_dir}/output"
    modified_file="${state_dir}/modified"
    created_file="${state_dir}/created"
    mkdir -p "${output_dir}"

    discover_patches "${patches_file}"
    parse_assignments "${assignments_file}" "$@"
    add_new_patch_entries "${patches_file}" "${assignments_file}" "${allow_new}"
    base=$(git -C "${OTP_DIR}" rev-parse "${BASE_REF}")
    tip=$(feature_tip "${patches_file}" 2>/dev/null || true)
    if [[ -z "${tip}" ]]; then
        local existing_file="${state_dir}/existing"
        discover_patches "${existing_file}"
        tip=$(feature_tip "${existing_file}") || error "OTP feature-stack metadata does not match existing patches."
    fi

    while IFS=$'\t' read -r commit _; do
        git -C "${OTP_DIR}" merge-base --is-ancestor "${tip}" "${commit}" || error "Assigned commit is not above the feature stack: ${commit}"
        git -C "${OTP_DIR}" merge-base --is-ancestor "${commit}" HEAD || error "Assigned commit is not in the current modification stack: ${commit}"
    done < "${assignments_file}"

    rewrite_assignment_trailers "${tip}" "${assignments_file}" "${rewritten_file}" "${commits_range_file}"

    local build_dir="${state_dir}/build"
    git clone --quiet --shared --no-checkout "${OTP_DIR}" "${build_dir}"
    git -C "${build_dir}" checkout --quiet --detach "${base}"
    build_regenerated_stack "${build_dir}" "${patches_file}" "${rewritten_file}" "${output_dir}" "${regenerated_commits_file}"
    regenerated_tip=$(git -C "${build_dir}" rev-parse HEAD)
    verify_regenerated_stack "${build_dir}" "${base}" "${patches_file}" "${output_dir}" "${regenerated_tip}"

    publish_patches "${patches_file}" "${output_dir}" "${modified_file}" "${created_file}"
    git -C "${OTP_DIR}" fetch --quiet "${build_dir}" "${regenerated_tip}"
    git -C "${OTP_DIR}" reset --hard --quiet "${regenerated_tip}"
    git -C "${OTP_DIR}" update-ref "${BASE_REF}" "${base}"
    while IFS=$'\t' read -r name commit; do
        git -C "${OTP_DIR}" update-ref "${PATCH_REF_PREFIX}/${name}" "${commit}"
    done < "${regenerated_commits_file}"
    rm -f "${OTP_DIR}/.stamp-autoconf"
    patches_digest=$(patch_hash "${patches_file}")
    printf '%s\n' "${patches_digest}" > "${STAMP_FILE}"

    print_patch_group "Modified patches" "${modified_file}"
    printf '\n'
    print_patch_group "Created patches" "${created_file}"
    rm -rf "${state_dir}"
    trap - EXIT
}

main() {
    local regen=false create_new=false
    local assignments=()

    while [[ $# -gt 0 ]]; do
        case "$1" in
            -h|--help) usage ;;
            --regen) regen=true ;;
            --create-new-patches) create_new=true ;;
            --*) error "Unknown option: $1" ;;
            *) assignments+=("$1") ;;
        esac
        shift
    done

    if [[ "${regen}" == "true" ]]; then
        regen_patches "${create_new}" "${assignments[@]}"
    else
        if [[ "${create_new}" == "true" ]] || [[ ${#assignments[@]} -gt 0 ]]; then
            error "Patch assignments and --create-new-patches require --regen."
        fi
        apply_patches
    fi
}

main "$@"
