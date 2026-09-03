import assert from "node:assert/strict";
import { cp, mkdtemp, rename, rm, stat } from "node:fs/promises";

const out = new URL("../../out/js/", import.meta.url);
const dist = new URL("../dist", import.meta.url);

for (const variant of ["core", "crypto"]) {
  assert((await stat(new URL(`runtimes/${variant}`, out))).isDirectory());
}

const stage = await mkdtemp(new URL("../.dist-", import.meta.url));
try {
  await cp(out, stage, { recursive: true, dereference: true });
  await rm(dist, { recursive: true, force: true });
  await rename(stage, dist);
} finally {
  await rm(stage, { recursive: true, force: true });
}
