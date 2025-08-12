import { getRoutePath, mdxModules } from ".";

export type NavigationTreeItem = {
  title?: string;
  path?: string;
  children?: NavigationTree;
  order?: number;
  type: "section" | "subsection";
};

export type NavigationTree = Record<string, NavigationTreeItem>;

export async function createNavigation(): Promise<NavigationTree> {
  const navItems: NavigationTree = {};

  const moduleEntries = Object.entries(mdxModules);
  const loadedModules = await Promise.all(
    moduleEntries.map(async ([path, loader]) => {
      const module = await loader();
      return {
        path,
        frontmatter: module.frontmatter
      };
    })
  );

  loadedModules.forEach(({ path, frontmatter }) => {
    let current = navItems;

    const routePath = getRoutePath(path);
    const segments = routePath
      .split("/")
      .filter((segment) => segment.length > 0);

    segments.forEach((segment, index) => {
      const title = segment.replaceAll("-", " ");
      const formattedTitle = title.charAt(0).toUpperCase() + title.slice(1);

      if (!current[segment]) {
        current[segment] = {
          title: formattedTitle,
          children: {},
          type: "section"
        };
      }

      if (!current[segment].children) {
        current[segment].children = {};
      }

      if (index === segments.length - 1) {
        current[segment] = {
          path: routePath,
          title: formattedTitle,
          order: frontmatter?.order || Infinity,
          children: {},
          type: "section"
        };

        frontmatter?.subsections?.forEach((subsection) => {
          const formattedTitle = subsection.replaceAll("-", " ");

          current[segment].children![subsection] = {
            title:
              formattedTitle.charAt(0).toUpperCase() + formattedTitle.slice(1),
            path: `${routePath}/#${subsection}`,
            type: "subsection"
          };
        });
      } else {
        current = current[segment].children;
      }
    });
  });

  return sortNavigationTree(navItems);
}

function sortNavigationTree(tree: NavigationTree): NavigationTree {
  const items = Object.entries(tree);

  items.sort(([, a], [, b]) => (a.order || Infinity) - (b.order || Infinity));

  items.forEach(([, item]) => {
    if (item.children && Object.keys(item.children).length > 0) {
      item.children = sortNavigationTree(item.children);
    }
  });

  const sortedTree: NavigationTree = {};
  items.forEach(([key, value]) => {
    sortedTree[key] = value;
  });

  return sortedTree;
}
