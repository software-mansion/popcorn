import type { ComponentType } from "react";

export type Frontmatter = {
  order?: number;
};

export type MdxWithProperties = {
  default: ComponentType;
  frontmatter?: Frontmatter;
  defaultCode?: string;
};

export type Dir = string;

export type DirEntry = {
  path: string[];
  children: DirTree;
  frontmatter: Frontmatter;
};

export type LoadedEntry = {
  path: string[];
  frontmatter: Frontmatter;
  hash64: string;
};

export type UnresolvedEntry = [string, () => Promise<MdxWithProperties>];

export type DirTree = Map<Dir, DirEntry>;

type NavigationTreeItemType = "link" | "section";

export type NavigationTreeItem = {
  title: string;
  path: string;
  parentPath: string;
  children: NavigationTreeItem[];
  type: NavigationTreeItemType;
};

export type NavigationTree = NavigationTreeItem[];
