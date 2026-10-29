// Progressive enhancement with Cross-Origin Storage (COS).
//
// The worker builds the list of files it would fetch (the runtime and the
// tarballs) and looks them up in COS by SHA-256. Where the worker has no
// `navigator.crossOriginStorage` but the page does, the worker asks the main
// thread to do the lookup and receives the hits as `Blob`s. Misses load over
// the network as before and are stored after boot by whichever context did
// the lookup. Without COS in either context none of this runs, and every COS
// failure falls back to the regular loading path.

import type { CosAsset, CosFiles } from "./types";

export const WASM_PATH = "beam.wasm";

/** Returns the COS API of the current context, page or worker. */
export function crossOriginStorage(): CrossOriginStorageManager | null {
  // Typed as always present, but most browsers don't have it yet.
  const nav = globalThis.navigator as Partial<Navigator> | undefined;
  const storage = nav?.crossOriginStorage;
  return typeof storage?.getFileHandle === "function" ? storage : null;
}

function toHash(sha256: string): CrossOriginStorageGetFileHandleHash {
  return { algorithm: "SHA-256", value: sha256 };
}

export async function lookupCos(
  storage: CrossOriginStorageManager,
  assets: CosAsset[],
): Promise<CosFiles> {
  const found = await Promise.all(
    assets.map(async (asset) => {
      try {
        const handle = await storage.getFileHandle(toHash(asset.sha256));
        return await handle.getFile();
      } catch {
        // `NotFoundError` on a miss; anything else also means "fetch it".
        return null;
      }
    }),
  );

  const files: CosFiles = {};
  assets.forEach((asset, i) => {
    const blob = found[i];
    if (blob) files[asset.key] = blob;
  });
  return files;
}

/**
 * Stores `assets` with `origins: "*"`. Uses the bytes in `loaded` where the
 * caller still has them, and otherwise reads them from the HTTP cache, which
 * has them from loading. COS hashes what it receives and rejects a mismatch.
 */
export async function storeInCos(
  storage: CrossOriginStorageManager,
  assets: CosAsset[],
  loaded: CosFiles = {},
): Promise<void> {
  for (const asset of assets) {
    try {
      let blob = loaded[asset.key];
      if (blob === undefined) {
        // A cached copy may already be stale, and revalidating it would cost
        // another request. COS's hash check catches any wrong bytes.
        const response = await fetch(asset.url, { cache: "force-cache" });
        if (!response.ok) continue;
        blob = await response.blob();
      }
      const handle = await storage.getFileHandle(toHash(asset.sha256), {
        create: true,
        origins: "*",
      });
      const writable = await handle.createWritable();
      await writable.write(blob);
      await writable.close();
    } catch {
      // Storing is best effort; the next visit just misses again.
    }
  }
}
