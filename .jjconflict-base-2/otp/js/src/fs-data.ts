import { err, type Result } from "./errors";
import { fetchBinary, fetchJson } from "./utils";

const CORE_APPS = new Set(["kernel", "stdlib", "compiler"]);

type BeamManifest = {
  entrypoint: string | null;
  apps: Record<string, { tar: string }>;
  vm: { boot: string };
};

export type LoadedFsData = {
  appNames: string[];
  entrypoint: string | null;
  bootFile: Uint8Array<ArrayBuffer>;
  tarballs: Uint8Array<ArrayBuffer>[];
};

export async function loadFsData(
  manifestUrl: string,
): Promise<Result<LoadedFsData>> {
  const manifest = await fetchJson<BeamManifest>(manifestUrl);
  if (manifest === null) {
    return {
      ok: false,
      error: err("beam:missing-manifest", { url: manifestUrl }),
    };
  }

  const appNames = Object.keys(manifest.apps);
  for (const name of CORE_APPS) {
    if (!Object.hasOwn(manifest.apps, name)) {
      return {
        ok: false,
        error: err("beam:missing-tarball", { name, all: appNames }),
      };
    }
  }

  const bootUrl = resolveManifestPath(manifestUrl, manifest.vm.boot);
  const bootFile = await fetchBinary(bootUrl);
  if (bootFile === null) {
    return {
      ok: false,
      error: err("beam:missing-boot-script", { url: bootUrl }),
    };
  }

  const loadedTarballs = await Promise.all(
    appNames.map(async (name): Promise<Result<Uint8Array<ArrayBuffer>>> => {
      const entry = manifest.apps[name];
      const tarUrl = resolveManifestPath(manifestUrl, entry.tar);
      const tar = await fetchBinary(tarUrl);
      if (tar === null) {
        return {
          ok: false,
          error: err("beam:missing-tarball", { name, all: appNames }),
        };
      }

      return { ok: true, data: tar };
    }),
  );

  const tarballs: Uint8Array<ArrayBuffer>[] = [];
  for (const tarball of loadedTarballs) {
    if (!tarball.ok) {
      return { ok: false, error: tarball.error };
    }
    tarballs.push(tarball.data);
  }

  return {
    ok: true,
    data: { appNames, entrypoint: manifest.entrypoint ?? null, bootFile, tarballs },
  };
}

function resolveManifestPath(manifestUrl: string, path: string): string {
  if (path.startsWith("/") || isAbsoluteUrl(path)) return path;
  return new URL(path, new URL(manifestUrl, globalThis.location.href)).toString();
}

function isAbsoluteUrl(path: string): boolean {
  return /^[a-zA-Z][a-zA-Z\d+\-.]*:/.test(path);
}
