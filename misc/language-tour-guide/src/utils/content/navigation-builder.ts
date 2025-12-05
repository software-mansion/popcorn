import { navigationConfig } from "./configuration";
import {
  isConfigGroup,
  type NavigationConfig,
  type NavigationTreeItem
} from "./types";

const MAX_NAVIGATION_DEPTH = 5000;

function buildNavigationTree(
  config: NavigationConfig,
  parentPath: string = "",
  depth: number = 0
): NavigationTreeItem[] {
  if (depth >= MAX_NAVIGATION_DEPTH) {
    console.warn(
      `Navigation depth limit reached (${MAX_NAVIGATION_DEPTH}). Stopping recursion to prevent stack overflow.`
    );
    return [];
  }

  return config.map((item) => {
    if (isConfigGroup(item)) {
      const currentPath = parentPath ? `${parentPath}/${item.slug}` : item.slug;
      const children = buildNavigationTree(item.items, currentPath, depth + 1);

      return {
        title: item.group,
        path: currentPath,
        children: children,
        type: "group" as const
      };
    } else {
      const currentPath = parentPath ? `${parentPath}/${item.slug}` : item.slug;

      return {
        title: item.name,
        path: currentPath,
        children: [],
        type: "link" as const
      };
    }
  });
}

type FlattenedNavigationItem = NavigationTreeItem & {
  siblingCount: number;
  indexInParent: number;
};

function flattenTreeToLinks(
  tree: NavigationTreeItem[]
): FlattenedNavigationItem[] {
  console.log(tree);
  const result: FlattenedNavigationItem[] = [];

  function traverse(items: NavigationTreeItem[]) {
    const linkChildren = items.filter((item) => item.type === "link");
    const siblingCount = linkChildren.length;

    let indexInParent = 0;

    for (const item of items) {
      if (item.type === "link") {
        result.push({
          ...item,
          siblingCount,
          indexInParent: indexInParent++
        });
      }
      if (item.children.length > 0) {
        traverse(item.children);
      }
    }
  }

  traverse(tree);
  return result;
}

export function getNodeNavigationSiblings(path: string) {
  const normalizePath = path.startsWith("/") ? path.slice(1) : path;

  const currentIndex = navigationLinks.findIndex(
    (item) => item.path === normalizePath
  );

  if (currentIndex === -1) {
    return { previousNode: null, nextNode: null };
  }

  const currentNode = navigationLinks[currentIndex];

  const nextNode =
    currentIndex + 1 < navigationLinks.length
      ? navigationLinks[currentIndex + 1]
      : null;

  const previousNode =
    currentIndex - 1 >= 0 ? navigationLinks[currentIndex - 1] : null;

  return {
    siblingsNode: {
      previousNode,
      nextNode
    },
    childrenCount: currentNode.siblingCount,
    currentIndex: currentNode.indexInParent + 1
  };
}

export const navigationTree = buildNavigationTree(navigationConfig);
const navigationLinks = flattenTreeToLinks(navigationTree);
