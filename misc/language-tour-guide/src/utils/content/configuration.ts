import { createElement, lazy } from "react";
import { MdxWrapper } from "../../components/markdown/MdxWrapper";
import type { NavigationConfig } from "./types";

const defaultConfigurationContent = (name: string) => {
  const LazyComponent = lazy(async () => {
    const module = await import(`../../content/${name}.livemd`);

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

export const navigationConfig: NavigationConfig = [
  {
    group: "Basic types and operations",
    slug: "basic-types-and-operations",
    items: [
      {
        name: "Atoms",
        slug: "atoms",
        component: defaultConfigurationContent("atoms")
      },
      {
        name: "Enum Details",
        slug: "enum",
        component: defaultConfigurationContent("enum")
      }
    ]
  },
  {
    group: "Tests",
    slug: "tests",
    items: [
      {
        name: "Test",
        slug: "test",
        component: defaultConfigurationContent("test")
      },
      {
        name: "Ranges",
        slug: "ranges",
        component: defaultConfigurationContent("ranges")
      }
    ]
  }
];
