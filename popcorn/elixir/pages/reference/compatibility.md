# Compatibility and browser limits

Popcorn uses a real BEAM virtual machine, but it runs inside a browser sandbox.
Test each dependency against the selected Popcorn release.

## Runtime capabilities

| Capability                        | Status        | Notes                                                          |
| --------------------------------- | ------------- | -------------------------------------------------------------- |
| BEAM bytecode                     | Supported     | The packager includes compiled application `ebin` directories. |
| Processes and message passing     | Supported     | Standard local BEAM semantics apply.                           |
| GenServer and supervisors         | Supported     | Popcorn applications use the standard OTP lifecycle.           |
| Timers and monitors               | Supported     | Browser suspension can delay wall-clock work.                  |
| Browser HTTP                      | Supported     | Use `Popcorn.Fetch` or its Req adapter.                        |
| Terminal input and output         | Supported     | Use `writeStdin`, output handlers, and `resizeTty`.            |
| `crypto`, `public_key`, and `ssl` | Optional      | Use the `crypto` runtime variant.                              |
| Dynamic NIF loading               | Not supported | Use native support compiled into the selected runtime.         |
| Native TCP and UDP sockets        | Not supported | Browser networking rules apply.                                |
| Erlang distribution               | Not supported | Use an application protocol over browser networking.           |
| Operating-system subprocesses     | Not supported | The browser cannot start local commands.                       |
| Application `priv` files          | Manual        | The packager does not copy them.                               |
| Mix runtime configuration         | Manual        | The packager does not transfer it into the browser.            |

## Browser memory

Each VM starts with 64 MiB of shared Wasm linear memory. It can grow to 256 MiB
on phones and tablets, or 2 GiB on desktop. Popcorn uses the browser's user agent
and touch support to select the platform, including iPads in desktop mode.
This is a device heuristic, not a measurement of available memory.

Configure the limits for an application that needs a different budget:

```javascript
const result = await Popcorn.init({
  beam: { maxMemory: { mobile: "512M", desktop: "1G" } },
});
```

Omit `maxMemory` to keep the defaults. Each key can be overridden separately;
for example, `{ mobile: "512M" }` keeps the desktop limit at 2 GiB.
Only `M` and `G` suffixes are supported; they mean MiB and GiB.

Each limit must be a multiple of 64 KiB between 64 MiB and 2 GiB. It applies to
Wasm linear memory; JavaScript, the virtual filesystem, and browser resources
use additional memory. A browser can refuse an allocation below the configured
limit. Lower limits reduce the memory available to large BEAM workloads.

## Toolchain compatibility

Use the toolchain linked from the selected Popcorn release. The packager checks
the host OTP version against the runtime manifest. It also reports known
unsupported applications and version mismatches. It cannot detect every
runtime assumption.

A successful version check does not prove that every dependency works. An
application can still require an unsupported NIF or operating-system feature.

## Review dependencies

Before you package an application, check these items:

1. Check each dependency's OTP applications.
2. Search for `erlang:load_nif/2` and native build steps.
3. Check for direct socket, subprocess, and file-system access.
4. Check for required `priv` files.
5. Check runtime configuration and environment requirements.
6. Run the application in a production browser build.
