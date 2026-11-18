import type { EmscriptenModule } from "./types";

const LOOKUP_TIMEOUT_MS = 100;
const WRITE_TIMEOUT_MS = 30_000;
const EXTENSION_LOOKUP_TIMEOUT_MS = 1_000;

type Asset = { url: string; sha256?: string };
type ExtensionRequest =
  | {
      type: "lookup";
      hash: CrossOriginStorageGetFileHandleHash;
      reply: MessagePort;
    }
  | {
      type: "store";
      hash: CrossOriginStorageGetFileHandleHash;
      body: ReadableStream<Uint8Array<ArrayBuffer>>;
    };

export class Assets {
  private readonly storage: CrossOriginStorageManager | null;

  private readonly extensionPort: MessagePort | null;

  constructor(enabled = true, extensionPort?: MessagePort) {
    const storage = globalThis.navigator?.crossOriginStorage;
    this.storage =
      enabled && typeof storage?.getFileHandle === "function" ? storage : null;
    this.extensionPort =
      enabled && this.storage === null ? (extensionPort ?? null) : null;
    if (this.extensionPort === null) extensionPort?.close();
  }

  // TODO: Remove this relay once the native COS implementation lands.
  static extension(
    enabled = true,
  ): { port: MessagePort; close: () => void } | undefined {
    const storage = globalThis.navigator?.crossOriginStorage;
    if (!enabled || typeof storage?.getFileHandle !== "function") return;
    const channel = new MessageChannel();
    const lifetime = new AbortController();
    channel.port1.onmessage = ({ data }: MessageEvent<ExtensionRequest>) => {
      if (data.type === "lookup") {
        void lookup(storage, data.hash, EXTENSION_LOOKUP_TIMEOUT_MS)
          .then((file) => {
            if (!lifetime.signal.aborted) data.reply.postMessage(file);
            data.reply.close();
          })
          .catch(() => data.reply.close());
      } else {
        void store(storage, data.hash, data.body, lifetime.signal);
      }
    };
    return {
      port: channel.port2,
      close: () => {
        lifetime.abort();
        channel.port1.close();
        channel.port2.close();
      },
    };
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
    const storage = this.storage ?? this.extensionPort;
    if (storage !== null && hash !== null) {
      const file = await lookup(
        storage,
        hash,
        this.extensionPort ? EXTENSION_LOOKUP_TIMEOUT_MS : LOOKUP_TIMEOUT_MS,
      );
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
    if (storage !== null && hash !== null && response.body !== null) {
      const body = response.clone().body!;
      if (storage instanceof MessagePort) {
        try {
          storage.postMessage(
            { type: "store", hash, body } satisfies ExtensionRequest,
            [body],
          );
        } catch {
          void body.cancel().catch(() => {});
        }
      } else {
        void store(storage, hash, body);
      }
    }
    return consume(response);
  }
}

async function lookup(
  storage: CrossOriginStorageManager | MessagePort,
  hash: CrossOriginStorageGetFileHandleHash,
  timeoutMs = LOOKUP_TIMEOUT_MS,
): Promise<File | null> {
  let timer: ReturnType<typeof setTimeout> | undefined;
  let channel: MessageChannel | undefined;
  try {
    const file =
      storage instanceof MessagePort
        ? new Promise<File | null>((resolve) => {
            channel = new MessageChannel();
            channel.port1.onmessage = (event) => resolve(event.data);
            storage.postMessage(
              {
                type: "lookup",
                hash,
                reply: channel.port2,
              } satisfies ExtensionRequest,
              [channel.port2],
            );
          })
        : storage.getFileHandle(hash).then((handle) => handle.getFile());
    return await Promise.race([
      file,
      new Promise<null>((resolve) => {
        timer = setTimeout(() => resolve(null), timeoutMs);
      }),
    ]);
  } catch {
    return null;
  } finally {
    clearTimeout(timer);
    channel?.port1.close();
    channel?.port2.close();
  }
}

async function store(
  storage: CrossOriginStorageManager,
  hash: CrossOriginStorageGetFileHandleHash,
  body: ReadableStream<Uint8Array<ArrayBuffer>>,
  signal?: AbortSignal,
): Promise<void> {
  const reader = body.getReader();
  let writer: WritableStreamDefaultWriter<Uint8Array<ArrayBuffer>> | undefined;
  let expired = false;
  const cancel = () => {
    void reader.cancel().catch(() => {});
    void writer?.abort().catch(() => {});
  };
  const expire = () => {
    expired = true;
    cancel();
  };
  const timer = setTimeout(expire, WRITE_TIMEOUT_MS);
  signal?.addEventListener("abort", expire, { once: true });
  try {
    if (signal?.aborted) {
      expire();
      return;
    }
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
    signal?.removeEventListener("abort", expire);
  }
}
