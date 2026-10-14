# Interacting with terminal

By default, Popcorn start the VM with `-noshell`. If you need any tty features, pass `tty: {}` to the `Popcorn.init()`.

In both modes, you can use `onStdout` and `onStderr` to receive stdout and stderr from the VM. By default, this comes as strings but can be configured (`tty: {output: "bytes"}`) to return `Uint8Array`s.

```typescript
const popcorn = await Popcorn.init({
  onStdout: (text) => terminal.write(text),
  onStderr: (text) => terminal.write(text),
});
```

## Sending input

To send stdin to the VM, use `writeStdin(string | Uint8Array)`. Popcorn doesn't add newlines nor enter (add `\r` to input for enter).

You may only queue up to 64kB until you get `stdio:overflow` error from the method. This typically signifies that VM doesn't catch up with the input volume.

```typescript
terminal.onData((data) => {
  const result = popcorn.tryWriteStdin(data);
  if (!result.ok) console.error(result.error);
});
```

## Resizing the terminal

Send the current terminal dimensions after you resize it in the JavaScript:

```typescript
popcorn.resizeTty(terminal.cols, terminal.rows);
```

See `examples/iex-wasm` for a complete Ghostty Web integration.
