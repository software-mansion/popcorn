import { createElement, lazy } from "react";
import { MdxWrapper } from "../../components/markdown/MdxWrapper";
import {
  isConfigGroup,
  isConfigItem,
  type ConfigGroup,
  type ConfigItem,
  type LiveMdModule,
  type NavigationConfig,
  type NavigationStructure
} from "./types";
import { navigationStructure, isNavGroup } from "./navigation-structure";

const modules = import.meta.glob("/src/content/**/*.livemd");

const lazyLoadLiveMd = (contentPath: string) => {
  const LazyComponent = lazy(async () => {
    const modulePath = `/src/content/${contentPath}.livemd`;

    if (!modules[modulePath]) {
      throw new Error(`Module not found: ${modulePath}`);
    }

    const module = (await modules[modulePath]()) as LiveMdModule;

    return {
      default: () =>
        createElement(MdxWrapper, {
          codeSnippets: module.codeSnippets,
          Component: module.default
        })
    };
  });

  return LazyComponent;
};

export function getSlug(item: ConfigItem | ConfigGroup): string {
  let name = "";

  if (isConfigGroup(item)) {
    name = item.group;
  }

  if (isConfigItem(item)) {
    name = item.name;
  }

  return name.toLowerCase().replace(/\s+/g, "-");
}

function buildNavigationConfig(
  structure: NavigationStructure
): NavigationConfig {
  return structure.map((item) => {
    if (isNavGroup(item)) {
      return {
        group: item.group,
        items: item.items.map((child) => ({
          name: child.name,
          component: lazyLoadLiveMd(child.contentPath)
        }))
      };
    }

    return {
      name: item.name,
      component: lazyLoadLiveMd(item.contentPath)
    };
  });
}

export const navigationConfig: NavigationConfig =
  buildNavigationConfig(navigationStructure);
