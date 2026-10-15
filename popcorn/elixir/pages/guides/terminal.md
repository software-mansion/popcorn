# Interacting with terminal

By default, Popcorn starts the VM with `-noshell`. If you need any tty features, pass `tty: {}` to `Popcorn.init()`.

In both modes, you can use `onStdout` and `onStderr` to receive stdout and stderr from the VM. By default, this comes as strings but can be configured (`tty: {output: "bytes"}`) to return `Uint8Array`s.

```typescript
const popcorn = await Popcorn.init({
  onStdout: (text) => terminal.write(text),
  onStderr: (text) => terminal.write(text),
});
```

## Sending input

To send stdin to the VM, use `writeStdin(string | Uint8Array)`. Popcorn doesn't add newline or Enter characters (add `\r` to input for enter).

You may only queue up to 64 KiB before you get a `stdio:overflow` error from the method. This typically signifies that the VM can't keep up with the input volume.

```typescript
terminal.onData((data) => {
  try {
    popcorn.writeStdin(data);
  } catch (error) {
    console.error(error);
  }
});
```

## Resizing the terminal

Send the current terminal dimensions after you resize it in JavaScript:

```typescript
popcorn.resizeTty(terminal.cols, terminal.rows);
```

See [`examples/iex-wasm`](https://github.com/software-mansion/popcorn/tree/main/examples/iex-wasm) for a complete Ghostty Web integration.
