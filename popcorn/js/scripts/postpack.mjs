import { rm, symlink } from "node:fs/promises";

const dist = new URL("../dist", import.meta.url);
await rm(dist, { recursive: true, force: true });
await symlink("../out/js", dist, "dir");
