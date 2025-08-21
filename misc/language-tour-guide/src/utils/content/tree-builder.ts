import { load, mdxModules, sortPathLength } from "./mdx-loader";
import type { DirEntry, DirTree } from "./types";

export async function getRawMdxTree() {
  const modules = Object.entries(mdxModules);
  const loadedFlat = await Promise.all(modules.map(load));
  loadedFlat.sort(sortPathLength);

  const tree: DirTree = new Map();

  for (const entry of loadedFlat) {
    let current = tree;

    for (let i = 0; i < entry.path.length - 1; i++) {
      const dir = entry.path[i];

      if (!current.has(dir)) {
        current.set(dir, {
          path: entry.path.slice(0, i + 1),
          children: new Map(),
          frontmatter: { order: Infinity, subsections: [], defaultCode: "" }
        });
      }

      current = current.get(dir)!.children;
    }

    const lastSegment = entry.path[entry.path.length - 1];
    current.set(lastSegment, {
      path: entry.path,
      children: new Map(),
      frontmatter: entry.frontmatter
    });
  }

  return tree;
}

export async function getNode(path: string[]): Promise<DirEntry | undefined> {
  const tree = await getRawMdxTree();
  const firstNode = tree.get(path[0]);

  if (firstNode === undefined) return undefined;

  let current: DirEntry = firstNode;

  for (const segment of path.slice(1)) {
    const entry = current?.children.get(segment);
    if (!entry) return undefined;
    current = entry;
  }

  return current;
}
