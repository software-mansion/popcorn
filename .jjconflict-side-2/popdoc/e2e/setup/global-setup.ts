import { spawn, ChildProcess, execSync } from "child_process";
import { resolve, dirname } from "path";
import { fileURLToPath } from "url";

declare global {
  var __POPDOC_SERVER_PROCESS__: ChildProcess | undefined;
}

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

const STARTUP_TIMEOUT_MS = 60_000;
const fixtureDir = resolve(__dirname, "../fixture");

function runCommand(command: string, cwd: string, description: string) {
  console.log(`e2e: ${description}...`);
  execSync(command, { cwd, stdio: "inherit" });
}

async function globalSetup() {
  runCommand("mix deps.get", fixtureDir, "fetching fixture Elixir deps");
  runCommand("mix docs", fixtureDir, "building fixture docs");

  console.log("e2e: starting Popdoc server...");

  const serverProcess = spawn("mix", ["popdoc.server"], {
    cwd: fixtureDir,
    stdio: ["pipe", "pipe", "pipe"],
    shell: true,
  });
  globalThis.__POPDOC_SERVER_PROCESS__ = serverProcess;

  await new Promise<void>((resolve, reject) => {
    const timeout = setTimeout(() => {
      reject(new Error("e2e: popdoc server startup timed out"));
    }, STARTUP_TIMEOUT_MS);

    let ready = false;
    const onStdout = (data: Buffer) => {
      const output = data.toString();
      if (!ready && output.includes("localhost:4000")) {
        ready = true;
        clearTimeout(timeout);
        resolve();
      }
    };

    serverProcess.stdout?.on("data", onStdout);
    serverProcess.stderr?.on("data", (data: Buffer) => {
      if (!ready) console.error(data.toString());
    });

    serverProcess.on("error", (err) => {
      clearTimeout(timeout);
      reject(err);
    });

    serverProcess.on("exit", (code) => {
      if (code !== 0 && code !== null && !ready) {
        clearTimeout(timeout);
        reject(new Error(`e2e: popdoc.server exited with code ${code}`));
      }
    });
  });

  console.log("e2e: popdoc server started on http://localhost:4000");
}

export default globalSetup;
