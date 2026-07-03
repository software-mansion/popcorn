import type { ShikiTransformer } from "shiki";

import Parser from "tree-sitter";

type HastElement = Parameters<NonNullable<ShikiTransformer["line"]>>[0];
import Elixir from "tree-sitter-elixir";

const parser = new Parser();
parser.setLanguage(Elixir);

export type LineAnnotation = { block: string | null; defpGroup?: string; defpHeader?: boolean };

export function parseElixirBlocks(code: string): LineAnnotation[] {
  const tree = parser.parse(code);
  const lineCount = code.split("\n").length;
  const annotations: LineAnnotation[] = Array.from(
    { length: lineCount },
    () => ({
      block: null,
    }),
  );

  const defpRanges: [number, number][] = [];
  walkNode(tree.rootNode, annotations, defpRanges);

  defpRanges.sort((a, b) => a[0] - b[0]);
  defpRanges.forEach(([start, end], i) => {
    const groupId = `defp-${i}`;
    annotations[start] = { ...annotations[start], defpGroup: groupId, defpHeader: true };
    for (let row = start + 1; row <= end; row++) {
      annotations[row] = { ...annotations[row], defpGroup: groupId };
    }
  });

  return annotations;
}

function walkNode(node: Parser.SyntaxNode, annotations: LineAnnotation[], defpRanges: [number, number][]) {
  if (node.type !== "call") {
    for (const child of node.children) walkNode(child, annotations, defpRanges);
    return;
  }

  const keyword = node.children[0]?.text as string | undefined;

  if (keyword === "defmodule") {
    annotations[node.startPosition.row] = { block: "module" };
    annotations[node.endPosition.row] = { block: "module" };
    // Mark `use X` lines inside the module body as module
    const doBlock = node.children.find((c: any) => c.type === "do_block");
    if (doBlock) {
      for (const child of doBlock.children) {
        if (child.type === "call" && child.children[0]?.text === "use") {
          annotations[child.startPosition.row] = { block: "module" };
        } else if (child.type === "call") {
          // Recurse into nested def/defp
          walkNode(child, annotations, defpRanges);
        }
      }
    }
    return;
  }

  if (keyword === "defp") {
    defpRanges.push([node.startPosition.row, node.endPosition.row]);
    return;
  }

  if (keyword === "def") {
    annotateDef(node, annotations);
    return;
  }

  for (const child of node.children) walkNode(child, annotations, defpRanges);
}

function annotateDef(node: Parser.SyntaxNode, annotations: LineAnnotation[]) {
  const outerArgs = node.children.find(
    (c: Parser.SyntaxNode) => c.type === "arguments",
  );
  if (!outerArgs) return;

  const sigCall = outerArgs.namedChildren?.[0] ?? outerArgs.children[0];
  if (!sigCall || sigCall.type !== "call") return;

  const funcName: string = sigCall.children[0]?.text ?? "";
  if (!funcName) return;

  let block = funcName;

  if (funcName === "handle_event") {
    const innerArgs = sigCall.children.find((c: any) => c.type === "arguments");
    const firstArg = innerArgs?.namedChildren?.[0] ?? innerArgs?.children?.[1];
    if (firstArg?.type === "string") {
      const qc = firstArg.children.find(
        (c: any) => c.type === "quoted_content",
      );
      block = qc?.text ?? firstArg.text.replace(/"/g, "");
    }
  } else if (funcName === "handle_info") {
    const innerArgs = sigCall.children.find((c: any) => c.type === "arguments");
    const firstArg = innerArgs?.namedChildren?.[0] ?? innerArgs?.children?.[1];
    if (firstArg?.type === "atom") {
      block = firstArg.text.replace(/^:/, "");
    }
  }

  const start: number = node.startPosition.row;
  const end: number = node.endPosition.row;
  for (let i = start; i <= end; i++) {
    annotations[i] = { block };
  }
}

export function createBlockTransformer(code: string): ShikiTransformer {
  const annotations = parseElixirBlocks(code);

  return {
    name: "llv-block-annotator",
    line(node: HastElement, line: number) {
      const ann = annotations[line - 1];

      const props = node.properties;
      if (ann?.block) props["data-block"] = ann.block;
      if (ann?.defpGroup) props["data-defp-group"] = ann.defpGroup;
      if (ann?.defpHeader) props["data-defp-header"] = "true";

      node.children = [
        { type: "element", tagName: "span", properties: {}, children: node.children },
      ];
    },
  };
}
