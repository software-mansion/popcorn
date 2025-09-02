import type { ComponentType } from "react";

export type Frontmatter = {
  order?: number;
  subsections?: string[];
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
};

export type UnresolvedEntry = [string, () => Promise<MdxWithProperties>];

export type DirTree = Map<Dir, DirEntry>;

type NavigationTreeItemType = "link" | "section" | "subsection";

export type NavigationTreeItem = {
  title: string;
  path: string;
  children: NavigationTreeItem[];
  type: NavigationTreeItemType;
};

export type NavigationTree = NavigationTreeItem[];
