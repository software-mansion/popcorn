import type { LoadedEntry, MdxWithProperties, UnresolvedEntry } from "./types";

export const mdxModules = import.meta.glob<MdxWithProperties>(
  "/src/content/**/*.mdx"
);

export async function load([
  rawPath,
  loader
]: UnresolvedEntry): Promise<LoadedEntry> {
  const { frontmatter } = await loader();
  const path = getRoutePath(rawPath).split("/");

  return {
    path,
    frontmatter: frontmatter ?? {
      order: Infinity,
      subsections: [],
    }
  };
}

export function getRoutePath(path: string): string {
  return path.replace("/src/content/", "").replace(/\.mdx$/, "");
}

export function sortPathLength(infoA: LoadedEntry, infoB: LoadedEntry) {
  return infoA.path.length - infoB.path.length;
}
