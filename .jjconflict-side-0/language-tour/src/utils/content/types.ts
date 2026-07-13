import type { ComponentType, ReactElement } from "react";
import type { CodeSnippet } from "../../plugins/livemd/parser";
import type { MdxWrapperProps } from "../../components/markdown/MdxWrapper";

export type LiveMdModule = {
  default: ComponentType;
  codeSnippets: CodeSnippet[];
};

export function isConfigItem(
  item: ConfigItem | ConfigGroup
): item is ConfigItem {
  return (item as ConfigItem).component !== undefined;
}

export function isConfigGroup(
  item: ConfigItem | ConfigGroup
): item is ConfigGroup {
  return (item as ConfigGroup).items !== undefined;
}

export type NavItem = {
  name: string;
  contentPath: string;
};

export type NavGroup = {
  group: string;
  items: NavItem[];
};

export type NavigationStructure = (NavItem | NavGroup)[];

export type ConfigItem = Omit<NavItem, "contentPath"> & {
  component: LazyComponent;
};

export type ConfigGroup = Omit<NavGroup, "items"> & {
  items: ConfigItem[];
};

export type NavigationConfig = (ConfigItem | ConfigGroup)[];

type LazyComponent = React.LazyExoticComponent<
  () => ReactElement<MdxWrapperProps, React.FunctionComponent<MdxWrapperProps>>
>;

export type RouteItem = {
  path: string;
  component: LazyComponent;
};

type NavigationTreeItemType = "link" | "group";

export type NavigationTreeItem = {
  title: string;
  path: string;
  children: NavigationTreeItem[];
  type: NavigationTreeItemType;
};

export type NavigationTree = NavigationTreeItem[];
