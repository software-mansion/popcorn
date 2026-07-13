const KILL_TIMEOUT_MS = 5_000;

async function globalTeardown() {
  const serverProcess = globalThis.__POPDOC_SERVER_PROCESS__;

  if (serverProcess !== undefined) {
    console.log("e2e: stopping Popdoc server...");
    serverProcess.kill("SIGTERM");

    await new Promise<void>((resolve) => {
      const timeout = setTimeout(() => {
        serverProcess.kill("SIGKILL");
        resolve();
      }, KILL_TIMEOUT_MS);

      serverProcess.on("exit", () => {
        clearTimeout(timeout);
        resolve();
      });
    });

    console.log("e2e: popdoc server stopped");
  }
}

export default globalTeardown;
