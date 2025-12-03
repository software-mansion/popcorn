import { log } from "./utils";
// @ts-ignore
import init from "../assets/AtomVM.mjs";
import { wasmPath } from "virtual:wasm";

let Module: any = null;

async function initAtomVM(): Promise<any> {
  log("Starting AtomVM initialization");

  try {
    // Get bundle path from meta tag set by popcorn
    const [bundlePath] = getMeta(["bundle-path"]);

    log(`Loading .avm bundle from: ${bundlePath}`);

    // Fetch the .avm bundle
    const bundleBuffer = await fetch(bundlePath).then((resp) =>
      resp.arrayBuffer(),
    );
    const bundle = new Int8Array(bundleBuffer);

    log("WASM module loading...");

    // Initialize the AtomVM module with the bundle
    Module = await init({
      locateFile(_path: string) {
        return wasmPath;
      },
      preRun: [
        function ({ FS }: any) {
          FS.mkdir("/data");
          FS.writeFile("/data/bundle.avm", bundle);
        },
      ],
      arguments: ["/data/bundle.avm"],
      print(text: string) {
        log(`AtomVM stdout: ${text}`);
      },
      printErr(text: string) {
        log(`AtomVM stderr: ${text}`);
      },
      onAbort() {
        log("AtomVM aborted");
      },
    });

    log("AtomVM initialized successfully");
    return Module;
  } catch (error) {
    log(`Failed to initialize AtomVM: ${error}`);
    throw error;
  }
}

async function main(): Promise<void> {
  log("Iframe main() starting");

  try {
    log("Before WASM module initialization");
    const atomvm = await initAtomVM();
    log("After WASM module initialization - success");

    // Store atomvm instance globally if needed
    (window as any).atomvm = atomvm;
  } catch (error) {
    log(`After WASM module initialization - failed: ${error}`);
  }
}

function getMeta<Ks extends string[]>(keys: Ks): string[] {
  const values: string[] = [];
  for (const key of keys) {
    const meta = document.querySelector(`meta[name="${key}"]`);
    if (meta === null) {
      throw new Error(`${key} meta tag not found`);
    }
    const value = meta.getAttribute("content");
    if (value === null) {
      throw new Error(`${key} is empty`);
    }

    values.push(value);
  }
  return values;
}

// Call main at top level
main();
