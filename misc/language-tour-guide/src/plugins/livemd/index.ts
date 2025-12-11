import type { Plugin, ResolvedConfig } from "vite";
import { readFile } from "node:fs/promises";
import path from "path";
import { transformToMdx } from "./parser";

export function livemdPlugin(): Plugin {
  let rootPath: string;

  return {
    name: "vite-plugin-livemd-to-mdx",
    enforce: "pre",
    configResolved(config: ResolvedConfig) {
      rootPath = config.root;
    },

    resolveId(source) {
      if (source.endsWith(".livemd")) {
        return source + ".mdx";
      }
    },

    async load(id) {
      if (!id.endsWith(".livemd.mdx")) {
        return;
      }

      const livemdRelativePath = id.replace(/\.mdx$/, "");
      const livemdPath = path.join(rootPath, "src", livemdRelativePath);

      const livemdContent = await readFile(livemdPath, "utf-8");

      return transformToMdx(livemdContent);
    }
  };
}
