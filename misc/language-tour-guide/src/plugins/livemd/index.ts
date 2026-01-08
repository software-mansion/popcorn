import type { Plugin, ResolvedConfig } from "vite";
import { readFile, glob } from "node:fs/promises";
import path from "path";
import { transformToMdx } from "./parser";
import { H64 } from "../../utils/storage";

async function computeContentHash(contentDir: string): Promise<string> {
  const hash = H64.update("");

  for await (const file of glob("**/*.livemd", { cwd: contentDir })) {
    const content = await readFile(path.join(contentDir, file), "utf-8");
    hash.update(content);
  }

  return hash.digest().toString(16);
}

export function livemdPlugin(): Plugin {
  let rootPath: string;

  return {
    name: "vite-plugin-livemd-to-mdx",
    enforce: "pre",
    configResolved(config: ResolvedConfig) {
      rootPath = config.root;
    },

    async config() {
      const contentDir = path.join(process.cwd(), "src", "content");
      const contentHash = await computeContentHash(contentDir);

      return {
        define: {
          __CONTENT_HASH__: JSON.stringify(contentHash)
        }
      };
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

      const regex = /\/content\/.+\.livemd/;
      const absoluteLivemdPath = id.match(regex)?.[0];

      if (!absoluteLivemdPath) {
        throw new Error(`Could not determine .livemd path from id: ${id}`);
      }

      const livemdPath = path.join(rootPath, "src", absoluteLivemdPath);

      const livemdContent = await readFile(livemdPath, "utf-8");

      return transformToMdx(livemdContent);
    }
  };
}
