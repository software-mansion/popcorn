import { getRawMdxTree } from "./tree-builder";
import type {
  Dir,
  DirEntry,
  DirTree,
  NavigationTree,
  NavigationTreeItem
} from "./types";

export async function createNavigation(): Promise<NavigationTree> {
  const tree = await getRawMdxTree();

  return processNavigationLevel(tree);
}

function processNavigationLevel(mapTree: DirTree): NavigationTree {
  const result: NavigationTree = [];
  const entries: [Dir, DirEntry][] = Array.from(mapTree.entries());

  entries.sort(compareEntriesByOrder);

  for (const [key, entry] of entries) {
    const isLeafNode = entry.children.size === 0;

    const item: NavigationTreeItem = {
      title: formatTitle(key),
      path: "/" + entry.path.join("/"),
      parentPath: "/" + entry.path.slice(0, -1).join("/"),
      type: isLeafNode ? "link" : "section",
      children: processNavigationLevel(entry.children)
    };

    result.push(item);
  }

  return result;
}

function formatTitle(text: string): string {
  const formattedText = text.replaceAll("-", " ");

  return formattedText.charAt(0).toUpperCase() + formattedText.slice(1);
}

function getParentPath(path: string): string {
  const cleanPath = path.split("?")[0];

  return cleanPath.split("/").slice(0, -1).join("/");
}

function compareEntriesByOrder(a: [Dir, DirEntry], b: [Dir, DirEntry]): number {
  const [, entryA] = a;
  const [, entryB] = b;

  const orderA = entryA.frontmatter.order;
  const orderB = entryB.frontmatter.order;

  if ((orderA === undefined && orderB === undefined) || orderA === orderB) {
    return 0;
  }

  return (orderA || Infinity) - (orderB || Infinity);
}

export function getNodeNavigationSiblings(tree: NavigationTree, path: string) {
  const flatNavigation: { title: string; path: string; parentPath: string }[] =
    [];

  const traverse = (items: NavigationTree) => {
    for (const item of items) {
      if (item.type === "link") {
        flatNavigation.push({
          title: item.title,
          path: item.path,
          parentPath: item.parentPath
        });
      }

      traverse(item.children);
    }
  };

  traverse(tree);

  const parentPath = getParentPath(path);

  const children = flatNavigation.filter((item) => {
    return item.parentPath === parentPath;
  });

  const currentChildrenIndex = children.findIndex((item) => item.path === path);

  const navigationNodeIndex = flatNavigation.findIndex(
    (item) => path === item.path
  );

  if (navigationNodeIndex === -1) {
    return { previousNode: null, nextNode: null };
  }

  const nextNode =
    navigationNodeIndex + 1 < flatNavigation.length
      ? flatNavigation[navigationNodeIndex + 1]
      : null;

  const previousNode =
    navigationNodeIndex - 1 >= 0
      ? flatNavigation[navigationNodeIndex - 1]
      : null;

  return {
    siblingsNode: {
      previousNode,
      nextNode
    },
    childrenCount: children.length,
    currentIndex: currentChildrenIndex + 1
  };
}
