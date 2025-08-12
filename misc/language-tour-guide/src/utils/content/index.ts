import type { ComponentType } from "react";

const regex = /^\/src\/content(\/[^.]+)(?:\.[^.]+)$/;

export const mdxModules = import.meta.glob<{
  default: ComponentType;
  frontmatter?: { order: number; subsections: string[] };
}>("/src/content/**/*.mdx");

export function getRoutePath(path: string): string {
  const match = path.match(regex);

  if (match) {
    return match[1];
  }

  return path;
}
