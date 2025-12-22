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
    name: "Introduction",
    component: lazyLoadLiveMd("introduction")
  },
  {
    group: "Basic types and operations",
    items: [
      {
        name: "Basics",
        component: lazyLoadLiveMd("1-basic-types-and-operations/basics")
      },
      {
        name: "Numbers",
        component: lazyLoadLiveMd("1-basic-types-and-operations/numbers")
      },
      {
        name: "Booleans",
        component: lazyLoadLiveMd("1-basic-types-and-operations/booleans")
      },
      {
        name: "Strings",
        component: lazyLoadLiveMd("1-basic-types-and-operations/strings")
      },
      {
        name: "Atoms",
        component: lazyLoadLiveMd("1-basic-types-and-operations/atoms")
      },
      {
        name: "Comparison",
        component: lazyLoadLiveMd("1-basic-types-and-operations/comparison")
      }
    ]
  },
  {
    group: "Lists and tuples",
    items: [
      {
        name: "Lists",
        component: lazyLoadLiveMd("2-lists-and-tuples/lists")
      },
      {
        name: "Tuples",
        component: lazyLoadLiveMd("2-lists-and-tuples/tuples")
      },
      {
        name: "Lists or Tuples",
        component: lazyLoadLiveMd("2-lists-and-tuples/lists-or-tuples")
      }
    ]
  },
  {
    group: "Keyword lists and maps",
    items: [
      {
        name: "Keyword lists",
        component: lazyLoadLiveMd("3-keyword-lists-and-maps/keyword_lists")
      },
      {
        name: "Maps",
        component: lazyLoadLiveMd("3-keyword-lists-and-maps/maps")
      }
    ]
  },
  {
    name: "Pattern matching",
    component: lazyLoadLiveMd("4-pattern-matching/pattern_matching")
  }
];
