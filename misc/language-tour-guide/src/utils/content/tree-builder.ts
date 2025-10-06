import { manageDefaultCodeStorage } from "../storage";
import { load, mdxModules, sortPathLength } from "./mdx-loader";
import type { DirTree } from "./types";

export async function getRawMdxTree() {
  const modules = Object.entries(mdxModules);
  const loadedFlat = await Promise.all(modules.map(load));
  loadedFlat.sort(sortPathLength);

  const defaultCodeConcatenated = loadedFlat.reduce((acc: string, entry) => {
    acc += entry.hashDefaultCode ?? "";
    return acc;
  }, "");

  manageDefaultCodeStorage(defaultCodeConcatenated);

  const tree: DirTree = new Map();

  for (const entry of loadedFlat) {
    let current = tree;
    const formatPath = [];

    for (let i = 0; i < entry.path.length - 1; i++) {
      const dir = entry.path[i];

      const [formatDir, dirOrder] = getFormatDirWithOrder(dir);

      formatPath.push(formatDir);

      if (!current.has(formatDir)) {
        current.set(formatDir, {
          path: formatPath,
          children: new Map(),
          frontmatter: { order: dirOrder }
        });
      }

      current = current.get(formatDir)!.children;
    }

    const lastSegment = entry.path[entry.path.length - 1];
    current.set(lastSegment, {
      path: [...formatPath, lastSegment],
      children: new Map(),
      frontmatter: entry.frontmatter
    });
  }

  return tree;
}

function getFormatDirWithOrder(name: string): [string, number] {
  const result = name.match(/^(\d+)-(.*)/);
  const order = result ? parseInt(result[1], 10) : Infinity;
  const formatName = result ? result[2] : name;

  return [formatName, order];
}
