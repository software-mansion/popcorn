# Add terminal input and output

Popcorn can connect BEAM terminal input and output to a browser terminal.
The IEx example uses this API.

## Enable the terminal

Popcorn starts the VM with `-noshell`, so no Erlang shell runs. Pass `tty` to
`Popcorn.init` to start the shell and attach it to your terminal. Use `tty: {}`
for the default size:

```typescript
const result = await Popcorn.init({
  tty: { size: { columns: terminal.cols, rows: terminal.rows } },
});
```

`beam.extraArgs` cannot re-enable the shell, because `erl` has no flag that
cancels `-noshell`.

## Receive output

Pass output handlers when you start Popcorn:

```typescript
const result = await Popcorn.init({
  onStdout: (text) => terminal.write(text),
  onStderr: (text) => terminal.write(text),
});
```

Popcorn decodes output as streaming UTF-8 text by default. Set
`tty.output: "bytes"` when the terminal needs raw byte chunks.

## Send input

Forward terminal input with `writeStdin`:

```typescript
terminal.onData((data) => {
  const result = popcorn.writeStdin(data);
  if (!result.ok) console.error(result.error);
});
```

Popcorn does not add a newline. Send `"\r"` when the terminal user presses
Enter.

The input queue holds 64 KiB. `writeStdin` returns `stdio:overflow` when a write
does not fit.

## Resize the terminal

Send the current terminal dimensions after a resize:

```typescript
const result = popcorn.resizeTty(terminal.cols, terminal.rows);
if (!result.ok) throw result.error;
```

Each dimension must use a value from 1 through 65,535 (`0xFFFF`).

See `examples/iex-wasm` for a complete Ghostty Web integration.
