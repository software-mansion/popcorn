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
        name: "Basic Types",
        component: lazyLoadLiveMd("1-basic-types-and-operations/basic-types")
      },
      {
        name: "Basic Arithmetic",
        component: lazyLoadLiveMd(
          "1-basic-types-and-operations/basic-arithmetic"
        )
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
        name: "Booleans and nil",
        component: lazyLoadLiveMd(
          "1-basic-types-and-operations/booleans-and-nil"
        )
      },
      {
        name: "Strings",
        component: lazyLoadLiveMd("1-basic-types-and-operations/strings")
      },
      {
        name: "Strings pt 2",
        component: lazyLoadLiveMd("1-basic-types-and-operations/strings-pt-2")
      },
      {
        name: "Atoms",
        component: lazyLoadLiveMd("1-basic-types-and-operations/atoms")
      },
      {
        name: "Structural Comparison",
        component: lazyLoadLiveMd(
          "1-basic-types-and-operations/structural-comparison"
        )
      },
      {
        name: "Variables",
        component: lazyLoadLiveMd("1-basic-types-and-operations/variables")
      }
    ]
  },
  {
    group: "Lists and tuples",
    items: [
      {
        name: "Introduction to Lists",
        component: lazyLoadLiveMd(
          "2-lists-and-tuples/introduction-to-lists-in-elixir"
        )
      },
      {
        name: "List Operations",
        component: lazyLoadLiveMd(
          "2-lists-and-tuples/list-operations-in-elixir"
        )
      },
      {
        name: "Prepending to List",
        component: lazyLoadLiveMd("2-lists-and-tuples/prepending-to-list")
      },
      {
        name: "Accessing List Elements",
        component: lazyLoadLiveMd("2-lists-and-tuples/accessing-list-elements")
      },
      {
        name: "Charlists",
        component: lazyLoadLiveMd("2-lists-and-tuples/charlists")
      },
      {
        name: "Tuples",
        component: lazyLoadLiveMd("2-lists-and-tuples/tuples")
      },
      {
        name: "Updating a Tuple",
        component: lazyLoadLiveMd("2-lists-and-tuples/updating-a-tuple")
      },
      {
        name: "Lists or Tuples?",
        component: lazyLoadLiveMd("2-lists-and-tuples/lists-or-tuples")
      }
    ]
  }
];
