const KILL_TIMEOUT_MS = 5_000;

async function globalTeardown() {
  const viteProcess = globalThis.__VITE_PROCESS__;

  if (viteProcess !== undefined) {
    console.log("e2e: stopping Vite dev server...");
    viteProcess.kill("SIGTERM");

    await new Promise<void>((resolve) => {
      const timeout = setTimeout(() => {
        viteProcess.kill("SIGKILL");
        resolve();
      }, KILL_TIMEOUT_MS);

      viteProcess.on("exit", () => {
        clearTimeout(timeout);
        resolve();
      });
    });

    console.log("e2e: vite dev server stopped");
  }
}

export default globalTeardown;
