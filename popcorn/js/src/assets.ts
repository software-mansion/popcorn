import type { EmscriptenModule } from "./types";

const LOOKUP_TIMEOUT_MS = 100;
const WRITE_TIMEOUT_MS = 30_000;

type Asset = { url: string; sha256?: string };

export class Assets {
  private readonly storage: CrossOriginStorageManager | null;

  constructor(enabled = true) {
    const storage = globalThis.navigator?.crossOriginStorage;
    this.storage =
      enabled && typeof storage?.getFileHandle === "function" ? storage : null;
  }

  binary(asset: Asset): Promise<Uint8Array<ArrayBuffer>> {
    return this.load(
      asset,
      async (response) => new Uint8Array(await response.arrayBuffer()),
    );
  }

  async wasm(
    url: string,
    sha256?: string,
  ): Promise<Required<Pick<EmscriptenModule, "instantiateWasm">>> {
    const module = await this.load({ url, sha256 }, (response) =>
      WebAssembly.compileStreaming(
        new Response(response.body, {
          headers: { "Content-Type": "application/wasm" },
        }),
      ),
    );
    return {
      instantiateWasm: (imports, receive) => {
        const instance = new WebAssembly.Instance(module, imports);
        receive(instance, module);
        return instance.exports;
      },
    };
  }

  private async load<T>(
    asset: Asset,
    consume: (response: Response) => Promise<T>,
  ): Promise<T> {
    const { url, sha256 } = asset;
    const hash =
      sha256 === undefined ? null : { algorithm: "SHA-256", value: sha256 };
    if (this.storage !== null && hash !== null) {
      const file = await lookup(this.storage, hash);
      if (file !== null) {
        try {
          return await consume(new Response(file));
        } catch {
          // A cached file can fail while being read or compiled.
        }
      }
    }

    const response = await fetch(url);
    if (!response.ok)
      throw new Error(`Failed to load ${url}: HTTP ${response.status}`);
    if (this.storage !== null && hash !== null && response.body !== null)
      void store(this.storage, hash, response.clone().body!);
    return consume(response);
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
  body: ReadableStream<Uint8Array<ArrayBuffer>>,
): Promise<void> {
  const reader = body.getReader();
  let writer: WritableStreamDefaultWriter<Uint8Array<ArrayBuffer>> | undefined;
  let expired = false;
  const cancel = () => {
    void reader.cancel().catch(() => {});
    void writer?.abort().catch(() => {});
  };
  const timer = setTimeout(() => {
    expired = true;
    cancel();
  }, WRITE_TIMEOUT_MS);
  try {
    const handle = await storage.getFileHandle(hash, {
      create: true,
      origins: "*",
    });
    if (expired) return;
    const writable = await handle.createWritable();
    writer = writable.getWriter();
    if (expired) {
      cancel();
      return;
    }
    while (true) {
      const { value, done } = await reader.read();
      if (expired) return;
      if (done) {
        await writer.close();
        return;
      }
      await writer.write(value);
    }
  } catch {
    cancel();
  } finally {
    clearTimeout(timer);
  }
}
