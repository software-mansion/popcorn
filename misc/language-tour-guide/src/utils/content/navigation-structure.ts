import type { NavGroup, NavigationStructure, NavItem } from "./types";

export const navigationStructure: NavigationStructure = [
  {
    name: "Introduction",
    contentPath: "introduction"
  },
  {
    group: "Basic types and operations",
    items: [
      { name: "Basics", contentPath: "1-basic-types-and-operations/basics" },
      { name: "Numbers", contentPath: "1-basic-types-and-operations/numbers" },
      {
        name: "Booleans",
        contentPath: "1-basic-types-and-operations/booleans"
      },
      { name: "Strings", contentPath: "1-basic-types-and-operations/strings" },
      { name: "Atoms", contentPath: "1-basic-types-and-operations/atoms" },
      {
        name: "Comparison",
        contentPath: "1-basic-types-and-operations/comparison"
      }
    ]
  },
  {
    group: "Lists and tuples",
    items: [
      { name: "Lists", contentPath: "2-lists-and-tuples/lists" },
      { name: "Tuples", contentPath: "2-lists-and-tuples/tuples" },
      {
        name: "Lists or Tuples",
        contentPath: "2-lists-and-tuples/lists-or-tuples"
      }
    ]
  },
  {
    group: "Keyword lists and maps",
    items: [
      {
        name: "Keyword lists",
        contentPath: "3-keyword-lists-and-maps/keyword_lists"
      },
      { name: "Maps", contentPath: "3-keyword-lists-and-maps/maps" }
    ]
  },
  {
    name: "Pattern matching",
    contentPath: "4-pattern-matching/pattern_matching"
  },
  {
    group: "Constructs",
    items: [
      { name: "case", contentPath: "5-constructs/case" },
      { name: "if and cond", contentPath: "5-constructs/if_and_cond" }
    ]
  },
  {
    name: "Anonymous functions",
    contentPath: "6-anonymous-functions/anonymous_functions"
  },
  {
    group: "Modules",
    items: [
      {
        name: "Modules and functions",
        contentPath: "7-modules/modules-and-functions"
      },
      {
        name: "Alias, require and import",
        contentPath: "7-modules/alias-require-import"
      },
      { name: "Module attributes", contentPath: "7-modules/module-attributes" },
      { name: "Behaviours", contentPath: "7-modules/behaviours" }
    ]
  },
  {
    group: "Processes",
    items: [
      { name: "Processes basics", contentPath: "8-processes/processes_basics" },
      { name: "Message passing", contentPath: "8-processes/message_passing" }
    ]
  },
  {
    name: "Congratulations",
    contentPath: "congratulations"
  }
];

export function isNavGroup(item: NavItem | NavGroup): item is NavGroup {
  return "group" in item;
}

function toSlug(name: string): string {
  return name.toLowerCase().replace(/\s+/g, "-");
}

export function getRoutes(): string[] {
  const routes: string[] = [];

  for (const item of navigationStructure) {
    if (isNavGroup(item)) {
      const groupSlug = toSlug(item.group);
      for (const child of item.items) {
        routes.push(`/${groupSlug}/${toSlug(child.name)}`);
      }
    } else {
      routes.push(`/${toSlug(item.name)}`);
    }
  }

  return routes;
}
