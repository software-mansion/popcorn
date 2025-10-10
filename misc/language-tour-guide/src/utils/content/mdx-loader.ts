import { hash64 } from "../storage";
import type { LoadedEntry, MdxWithProperties, UnresolvedEntry } from "./types";

export const mdxModules = import.meta.glob<MdxWithProperties>(
  "/src/content/**/*.mdx"
);

export async function load([
  rawPath,
  loader
]: UnresolvedEntry): Promise<LoadedEntry> {
  const { frontmatter, defaultCode } = await loader();

  const path = getPath(rawPath).split("/");

  return {
    path,
    frontmatter: frontmatter ?? {
      order: Infinity
    },
    // We use hash64 for defaultCode to generate a unique content identifier with strong collision resistanc
    hash64: defaultCode ? hash64(defaultCode) : ""
  };
}

function getPath(path: string): string {
  return path.replace("/src/content/", "").replace(/\.mdx$/, "");
}

export function getNavigationPath(path: string): string {
  return getPath(path)
    .replace(/\/\d+-/g, "/")
    .replace(/^\d+-/, "");
}

export function sortPathLength(infoA: LoadedEntry, infoB: LoadedEntry) {
  return infoA.path.length - infoB.path.length;
}
