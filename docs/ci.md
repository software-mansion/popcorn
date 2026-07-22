# Continuous integration

Normal CI is defined in `.github/workflows/ci.yml`. The workflow always creates the final `CI` check, while `scripts/ci-changed-groups.sh` selects the suite groups affected by a change.

Only the final `CI` check should be required by branch protection. It succeeds when change detection succeeds and every selected group aggregator succeeds. A documentation-only change can legitimately run only change detection and this final check.

## Add or change a suite

1. Add the task job to `.github/workflows/ci.yml` and gate it with the appropriate output from `changes`.
2. Add the task to that group's `if: always()` aggregator. The aggregator must fail unless every selected prerequisite and task result is `success`.
3. If the task uses a browser, upload its report under a suite-specific artifact name when it fails.
4. Extend `scripts/ci-changed-groups.sh` when new paths should select the group, then add a representative case to `scripts/ci-changed-groups-test.sh`.
5. Run `scripts/ci-changed-groups-test.sh` and `actionlint .github/workflows/ci.yml`.

The groups are `core`, `browser_apps`, `llv`, `otp_no_crypto`, and `otp_crypto`. `atomvm_runtime` is an internal producer group selected whenever Core, Browser apps, or LocalLiveView needs an AtomVM artifact.

## FissionVM revision

CI does not pin FissionVM in this repository. At the start of each selected run, `fissionvm-revision` resolves the tip of the `swm` branch. Both the WASM and Unix producer jobs fetch that exact revision, so they cannot observe different commits within one run.

To test another FissionVM branch, change the ref passed to `git ls-remote` in `.github/workflows/ci.yml` and the fallback lookup in `.github/actions/build-atomvm/action.yml`. Restore both to `swm` after the test.

## Invalidate caches

- AtomVM keys include the resolved FissionVM commit, build mode, and a hash of the CMake options. A new FissionVM commit or CMake option creates a new entry automatically. For a forced rebuild, add a short cache-generation component to the key in `.github/actions/build-atomvm/action.yml`.
- OTP runtime keys include the OTP tag, build mode, crypto mode, and hashes of the patches and build scripts. Those inputs invalidate the cache automatically. For a forced rebuild, add a short cache-generation component to the computed keys in `.github/actions/build-otp-wasm/action.yml`.
- Mix keys include the runner OS, Elixir and OTP versions, `MIX_ENV`, and hashes of `mix.exs` and `mix.lock`. Change the explicit toolchain version or cache-name component when a manual reset is required.
- Playwright keys include the runner OS and browser version. Bump the version passed to `.github/actions/setup-playwright` to select a new browser cache.

Do not delete unrelated cache entries merely to force one variant to rebuild. GitHub cache entries are immutable; changing the relevant key component is sufficient.
