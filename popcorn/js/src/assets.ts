import type { EmscriptenModule } from "./types";

const LOOKUP_TIMEOUT_MS = 100;

type Asset = { url: string; sha256?: string };

export class Assets {
  private readonly storage: CrossOriginStorageManager | null;

  constructor(enabled = true) {
    const storage = globalThis.navigator?.crossOriginStorage;
    this.storage =
      enabled && typeof storage?.getFileHandle === "function" ? storage : null;
  }

  async fetch({ url, sha256 }: Asset): Promise<Response> {
    if (this.storage === null || sha256 === undefined) return fetch(url);

    const hash = { algorithm: "SHA-256", value: sha256 };
    const file = await lookup(this.storage, hash);
    if (file !== null) return new Response(file);

    const response = await fetch(url);
    if (response.ok && response.body !== null)
      void store(this.storage, hash, response.clone());
    return response;
  }

  wasm(
    sha256?: string,
  ): Required<Pick<EmscriptenModule, "instantiateWasmFromUrl">> {
    return {
      instantiateWasmFromUrl: async (url, imports) => {
        const response = await this.fetch({ url, sha256 });
        if (!response.ok) {
          throw new Error(`Failed to load ${url}: HTTP ${response.status}`);
        }
        return WebAssembly.instantiateStreaming(
          new Response(response.body, {
            headers: { "Content-Type": "application/wasm" },
          }),
          imports,
        );
      },
    };
  }
}

async function lookup(
  storage: CrossOriginStorageManager,
  hash: CrossOriginStorageGetFileHandleHash,
): Promise<File | null> {
  let timer: ReturnType<typeof setTimeout> | undefined;
  try {
    return await Promise.race([
      storage.getFileHandle(hash).then((handle) => handle.getFile()),
      new Promise<null>((resolve) => {
        timer = setTimeout(() => resolve(null), LOOKUP_TIMEOUT_MS);
      }),
    ]);
  } catch {
    return null;
  } finally {
    clearTimeout(timer);
  }
}

async function store(
  storage: CrossOriginStorageManager,
  hash: CrossOriginStorageGetFileHandleHash,
  response: Response,
): Promise<void> {
  try {
    const handle = await storage.getFileHandle(hash, {
      create: true,
      origins: "*",
    });
    const writable = await handle.createWritable();
    await response.body!.pipeTo(writable);
  } catch {
    // Release the clone if storage cannot consume it.
    void response.body!.cancel().catch(() => {});
  }
}
