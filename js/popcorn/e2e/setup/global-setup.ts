import { spawn, ChildProcess, execSync } from "child_process";
import { resolve, dirname } from "path";
import { fileURLToPath } from "url";

declare global {
  var __VITE_PROCESS__: ChildProcess | undefined;
}

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

const STARTUP_TIMEOUT_MS = 30_000;
const elixirDir = resolve(__dirname, "../elixir");

function runCommand(command: string, cwd: string, description: string) {
  console.log(`e2e: ${description}...`);
  execSync(command, { cwd, stdio: "inherit" });
}

async function globalSetup() {
  runCommand("mix deps.get", elixirDir, "fetching Elixir dependencies");
  runCommand("mix popcorn.cook", elixirDir, "creating Elixir bundle");

  console.log("e2e: starting Vite dev server...");

  const viteProcess = spawn("npx", ["vite"], {
    cwd: __dirname,
    stdio: ["ignore", "pipe", "pipe"],
    shell: true,
  });
  globalThis.__VITE_PROCESS__ = viteProcess;

  await new Promise<void>((resolve, reject) => {
    const timeout = setTimeout(() => {
      reject(new Error("e2e: vite server startup timed out"));
    }, STARTUP_TIMEOUT_MS);

    viteProcess.stdout?.on("data", (data: Buffer) => {
      const output = data.toString();
      console.log(output);
      if (output.includes("Local:") || output.includes("ready in")) {
        clearTimeout(timeout);
        resolve();
      }
    });

    viteProcess.stderr?.on("data", (data: Buffer) => {
      console.error(data.toString());
    });

    viteProcess.on("error", (err) => {
      clearTimeout(timeout);
      reject(err);
    });

    viteProcess.on("exit", (code) => {
      if (code !== 0 && code !== null) {
        clearTimeout(timeout);
        reject(new Error(`e2e: vite exited with code ${code}`));
      }
    });
  });

  console.log("e2e: vite dev server started on http://localhost:5173");
}

export default globalSetup;
