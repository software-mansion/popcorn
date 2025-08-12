import type { ComponentType } from "react";

export const mdxModules = import.meta.glob<{
  default: ComponentType;
  frontmatter?: { order: number; subsections: string[] };
}>("/src/content/**/*.mdx");

export function getRoutePath(path: string): string {
  return path.replace("/src/content", "").replace(/\.mdx$/, "");
}
