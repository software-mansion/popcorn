# Runtime model

Popcorn runs a BEAM virtual machine in a Web Worker. The page and the virtual
machine communicate through the Popcorn bridge.

```text
Browser page
  JavaScript application
          |
          | Popcorn bridge
          v
Web Worker
  BEAM virtual machine
    OTP application
      Supervisor
      GenServers and other processes
```

## Application code

Popcorn packages compiled `.beam` files. It does not convert application code
to JavaScript.

The bundler plugin reads the Mix build directory. It selects the entrypoint
application, required dependencies, and configured extra applications.

The virtual machine starts the entrypoint through the standard OTP application
lifecycle. `Popcorn.init()` waits for the application tree to start.

## Process isolation

BEAM processes run inside the virtual machine. They keep the normal OTP message
and supervision model.

The Web Worker also isolates the runtime from the page's main JavaScript thread.
Long-running BEAM work does not block the page's render loop. The page remains
responsible for its user interface.

## Communication

JavaScript can send messages to a registered name or PID. It can also call and
cast GenServers through `Popcorn.Proxy`.

Elixir can pass a PID to JavaScript with `Popcorn.Wasm.send/1` or
`Popcorn.Wasm.run_js/3`. JavaScript receives published events through
`onEvent()`.

Elixir can call JavaScript with `Popcorn.Wasm.run_js/3`. Popcorn evaluates the
function source in the page.

Do not pass untrusted text as the function source to `Popcorn.Wasm.run_js/3`.

Each bridge direction has a defined value conversion. See
[Values across the bridge](values.html).

## Browser sandbox

The WebAssembly runtime still follows browser security rules. HTTP uses browser
`fetch`, and cross-origin resource sharing (CORS) rules apply.

Popcorn cannot provide every operating-system feature from a native BEAM. See
[Compatibility and browser limits](compatibility.html).
