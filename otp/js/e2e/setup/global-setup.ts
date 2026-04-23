import { spawn, ChildProcess } from "child_process";
import { resolve, dirname } from "path";
import { fileURLToPath } from "url";

declare global {
  var __VITE_PROCESS__: ChildProcess | undefined;
}

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

const STARTUP_TIMEOUT_MS = 30_000;
const jsRootDir = resolve(__dirname, "../..");
const viteCliPath = resolve(__dirname, "node_modules/vite/bin/vite.js");

async function globalSetup() {
  const nodeExecutable = await resolveNodeExecutable();
  const env = withNodeOnPath(nodeExecutable);

  await runCommand(
    "pnpm",
    ["run", "build"],
    jsRootDir,
    "building JS library and OTP assets",
    env,
  );

  console.log("e2e: starting Vite dev server...");

  const viteProcess = spawn(nodeExecutable, [viteCliPath], {
    cwd: __dirname,
    stdio: ["ignore", "pipe", "pipe"],
    env,
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

  console.log("e2e: vite dev server started on http://127.0.0.1:5173");
}

export default globalSetup;

async function resolveNodeExecutable(): Promise<string> {
  try {
    const miseDir = await captureCommand("mise", ["where", "node"], jsRootDir);
    if (miseDir.length > 0) {
      return resolve(miseDir, "bin/node");
    }
  } catch {
    // Fall back to the current Node executable when mise is unavailable.
  }

  return process.execPath;
}

async function runCommand(
  command: string,
  args: string[],
  cwd: string,
  description: string,
  env: NodeJS.ProcessEnv,
): Promise<void> {
  console.log(`e2e: ${description}...`);

  await new Promise<void>((resolve, reject) => {
    const child = spawn(command, args, {
      cwd,
      stdio: "inherit",
      env,
    });

    child.on("error", (error) => reject(error));
    child.on("exit", (code, signal) => {
      if (code === 0) {
        resolve();
        return;
      }

      reject(
        new Error(
          signal === null
            ? `e2e: command exited with code ${code ?? "unknown"}`
            : `e2e: command exited with signal ${signal}`,
        ),
      );
    });
  });
}

async function captureCommand(
  command: string,
  args: string[],
  cwd: string,
): Promise<string> {
  return await new Promise<string>((resolve, reject) => {
    const child = spawn(command, args, {
      cwd,
      stdio: ["ignore", "pipe", "ignore"],
      env: process.env,
    });

    let stdout = "";

    child.stdout?.on("data", (data: Buffer) => {
      stdout += data.toString();
    });

    child.on("error", (error) => reject(error));
    child.on("exit", (code) => {
      if (code === 0) {
        resolve(stdout.trim());
        return;
      }

      reject(new Error(`e2e: command exited with code ${code ?? "unknown"}`));
    });
  });
}

function withNodeOnPath(nodeExecutable: string): NodeJS.ProcessEnv {
  return {
    ...process.env,
    PATH: `${dirname(nodeExecutable)}:${process.env.PATH ?? ""}`,
  };
}
