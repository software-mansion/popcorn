#!/usr/bin/env bash

set -euo pipefail

base_sha="${1:?base SHA is required}"
head_sha="${2:?head SHA is required}"
atomvm_runtime=false
core=false
browser_apps=false
llv=false
otp_no_crypto=false
otp_crypto=false

mark_all() {
  atomvm_runtime=true
  core=true
  browser_apps=true
  llv=true
  otp_no_crypto=true
  otp_crypto=true
}

if [[ "$base_sha" == "0000000000000000000000000000000000000000" ]]; then
  mark_all
else
  while IFS= read -r path; do
    case "$path" in
      .github/actions/* | .github/workflows/ci.yml | scripts/ci-changed-groups.sh)
        mark_all
        ;;
      pnpm-workspace.yaml | pnpm-lock.yaml | package.json | .npmrc)
        core=true
        browser_apps=true
        llv=true
        otp_no_crypto=true
        otp_crypto=true
        ;;
      popcorn/js/*)
        core=true
        browser_apps=true
        ;;
      popcorn/elixir/lib/*/wasm.ex)
        core=true
        browser_apps=true
        ;;
      popcorn/elixir/* | examples/hello-popcorn/* | examples/eval-in-wasm/*)
        core=true
        ;;
      popdoc/* | language-tour/*)
        browser_apps=true
        ;;
      local-live-view/* | examples/local-lv-*)
        llv=true
        ;;
      otp/elixir/* | examples/hello-popcorn-otp/*)
        otp_no_crypto=true
        ;;
      otp/js/* | otp/patches/* | scripts/build-beam.sh | scripts/build-openssl.sh | scripts/patch-beam.sh | scripts/stdlib.sh | scripts/_common.sh)
        otp_no_crypto=true
        otp_crypto=true
        ;;
    esac
  done < <(git diff --name-only "$base_sha" "$head_sha")

  if [[ "$core" == true || "$browser_apps" == true || "$llv" == true ]]; then
    atomvm_runtime=true
  fi
fi

write_outputs() {
  printf 'atomvm_runtime=%s\n' "$atomvm_runtime"
  printf 'core=%s\n' "$core"
  printf 'browser_apps=%s\n' "$browser_apps"
  printf 'llv=%s\n' "$llv"
  printf 'otp_no_crypto=%s\n' "$otp_no_crypto"
  printf 'otp_crypto=%s\n' "$otp_crypto"
}

if [[ -n "${GITHUB_OUTPUT:-}" ]]; then
  write_outputs >> "$GITHUB_OUTPUT"
else
  write_outputs
fi
