import { Popcorn } from "@swmansion/popcorn";
import { FitAddon, init, Terminal } from "ghostty-web";

const LANGUAGE = document.querySelector('meta[name="code-language"]').content;

// IEx needs Elixir's user driver; erl gets the default shell the boot script starts.
const SHELL_ARGS = {
  elixir: [
    "-elixir_root",
    "/lib",
    "-user",
    "elixir",
    "-extra",
    "--no-halt",
    "+iex",
    "--dot-iex",
    "",
  ],
  erlang: [],
};

const status = document.getElementById("status");
const terminalElement = document.getElementById(`${LANGUAGE}-terminal`);

await init();

const terminal = new Terminal({
  cursorBlink: true,
  scrollback: 10_000,
});
const fitAddon = new FitAddon();
terminal.loadAddon(fitAddon);
terminal.open(terminalElement);
fitAddon.fit();
fitAddon.observeResize();
terminal.focus();

const focusTerminal = () => {
  terminal.focus();
  terminal.textarea?.focus();
};

terminalElement.addEventListener("mousedown", focusTerminal);
terminalElement.addEventListener("touchstart", focusTerminal, {
  passive: true,
});

terminal.writeln("\x1b[90mBooting BEAM and starting the shell…\x1b[0m");

const result = await Popcorn.init({
  beam: {
    manifestUrl: "/assets/otp/manifest.json",
    env: { TERM: "xterm-256color" },
    extraArgs: SHELL_ARGS[LANGUAGE],
  },
  tty: { size: ttySize() },
  onStdout: (chunk) => terminal.write(chunk),
  onStderr: (chunk) => terminal.write(chunk),
  onError: (error) => {
    terminal.writeln(`\r\n\x1b[31mVM error: ${JSON.stringify(error)}\x1b[0m`);
  },
});

if (!result.ok) {
  status.textContent = "BEAM failed to boot";
  terminal.writeln(`\r\n\x1b[31m${result.error.message}\x1b[0m`);
  throw result.error;
}

const popcorn = result.data;
status.textContent = "BEAM booted · interactive shell ready";
globalThis.iexWasm = { popcorn, terminal };
focusTerminal();

// Resizes emitted while the VM was booting had nowhere to go yet.
fitAddon.fit();
popcorn.resizeTty(terminal.cols, terminal.rows);

terminal.onData((data) => {
  const input = popcorn.writeStdin(data);
  if (!input.ok) status.textContent = input.error.message;
});

terminal.onResize(({ cols, rows }) => {
  const resize = popcorn.resizeTty(cols, rows);
  if (!resize.ok) status.textContent = resize.error.message;
});

document.documentElement.dataset.popcornReady = "true";

// Popcorn rejects a zero-sized TTY, which is what we read before layout settles.
function ttySize() {
  const columns = terminal.cols || 80;
  const rows = terminal.rows || 24;
  return { columns, rows };
}
