import { createElement, lazy } from "react";
import { MdxWrapper } from "../../components/markdown/MdxWrapper";
import {
  isConfigGroup,
  isConfigItem,
  type ConfigGroup,
  type ConfigItem,
  type LiveMdModule,
  type NavigationConfig
} from "./types";

const modules = import.meta.glob("/src/content/**/*.livemd");

const lazyLoadLiveMd = (name: string) => {
  const LazyComponent = lazy(async () => {
    const modulePath = `/src/content/${name}.livemd`;

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

export const navigationConfig: NavigationConfig = [
  {
    group: "Basic types and operations",
    items: [
      {
        name: "Atoms",
        component: defaultConfigurationContent("atoms")
      },
      {
        name: "Enum Details",
        component: defaultConfigurationContent("enum")
      }
    ]
  },
  {
    group: "Tests",
    items: [
      {
        name: "Test",
        component: defaultConfigurationContent("test")
      },
      {
        name: "Ranges",
        component: defaultConfigurationContent("ranges")
      }
    ]
  }
];
