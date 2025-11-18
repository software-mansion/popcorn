import type { Plugin, ResolvedConfig } from "vite";
import fs from "fs";
import path from "path";
import { parseLivemd } from "./parser";

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
      if (id.endsWith(".livemd.mdx")) {
        const livemdPath = id.replace(/\.mdx$/, "");

        // Read the .livemd file
        const livemdContent = fs.readFileSync(
          path.join(rootPath, "src", livemdPath),
          "utf-8"
        );

        const result = await parseLivemd(livemdContent);

        return String(result);
      }
    }
  };
}
