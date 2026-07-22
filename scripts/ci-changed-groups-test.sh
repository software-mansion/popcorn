#!/usr/bin/env bash

set -euo pipefail

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo_dir="$(mktemp -d)"
trap 'rm -rf "$repo_dir"' EXIT

git -C "$repo_dir" init -q
git -C "$repo_dir" config user.email ci@ci.local
git -C "$repo_dir" config user.name CI
touch "$repo_dir/README.md"
git -C "$repo_dir" add README.md
git -C "$repo_dir" commit -qm initial

assert_outputs() {
  local path="$1"
  local expected="$2"
  local base_sha
  local head_sha
  local output

  base_sha="$(git -C "$repo_dir" rev-parse HEAD)"
  mkdir -p "$repo_dir/$(dirname "$path")"
  touch "$repo_dir/$path"
  git -C "$repo_dir" add "$path"
  git -C "$repo_dir" commit -qm "add $path"
  head_sha="$(git -C "$repo_dir" rev-parse HEAD)"
  output="$(cd "$repo_dir" && "$script_dir/ci-changed-groups.sh" "$base_sha" "$head_sha")"

  if [[ "$output" != "$expected" ]]; then
    printf 'Unexpected outputs for %s:\n%s\n' "$path" "$output" >&2
    exit 1
  fi
}

all_false='atomvm_runtime=false
core=false
browser_apps=false
llv=false
otp_no_crypto=false
otp_crypto=false'

assert_outputs docs/ci.md "$all_false"
assert_outputs popcorn/js/src/index.ts 'atomvm_runtime=true
core=true
browser_apps=true
llv=false
otp_no_crypto=false
otp_crypto=false'
assert_outputs examples/local-lv-forms/mix.exs 'atomvm_runtime=true
core=false
browser_apps=false
llv=true
otp_no_crypto=false
otp_crypto=false'
assert_outputs otp/elixir/lib/popcorn.ex 'atomvm_runtime=false
core=false
browser_apps=false
llv=false
otp_no_crypto=true
otp_crypto=false'
assert_outputs scripts/build-beam.sh 'atomvm_runtime=false
core=false
browser_apps=false
llv=false
otp_no_crypto=true
otp_crypto=true'
assert_outputs .github/workflows/ci.yml 'atomvm_runtime=true
core=true
browser_apps=true
llv=true
otp_no_crypto=true
otp_crypto=true'

zero_output="$(cd "$repo_dir" && "$script_dir/ci-changed-groups.sh" 0000000000000000000000000000000000000000 HEAD)"
if [[ "$zero_output" != 'atomvm_runtime=true
core=true
browser_apps=true
llv=true
otp_no_crypto=true
otp_crypto=true' ]]; then
  printf 'Unexpected outputs for an initial branch push:\n%s\n' "$zero_output" >&2
  exit 1
fi
