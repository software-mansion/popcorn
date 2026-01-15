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
  },
  {
    group: "Constructs",
    items: [
      {
        name: "case",
        component: lazyLoadLiveMd("5-constructs/case")
      },
      {
        name: "if and cond",
        component: lazyLoadLiveMd("5-constructs/if_and_cond")
      }
    ]
  },
  {
    name: "Anonymous functions",
    component: lazyLoadLiveMd("6-anonymous-functions/anonymous_functions")
  },
  {
    group: "Modules",
    items: [
      {
        name: "Modules and functions",
        component: lazyLoadLiveMd("7-modules/modules-and-functions")
      },
      {
        name: "Alias, require and import",
        component: lazyLoadLiveMd("7-modules/alias-require-import")
      },
      {
        name: "Module attributes",
        component: lazyLoadLiveMd("7-modules/module-attributes")
      },
      {
        name: "Behaviours",
        component: lazyLoadLiveMd("7-modules/behaviours")
      }
    ]
  },
  {
    group: "Processes",
    items: [
      {
        name: "Processes basics",
        component: lazyLoadLiveMd("8-processes/processes_basics")
      },
      {
        name: "Message passing",
        component: lazyLoadLiveMd("8-processes/message_passing")
      }
    ]
  },
  {
    name: "Congratulations",
    component: lazyLoadLiveMd("congratulations")
  }
];
