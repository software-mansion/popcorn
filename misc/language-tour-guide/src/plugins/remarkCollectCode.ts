import { visit, SKIP, type Test } from "unist-util-visit";
import type { Node, Parent } from "unist";

interface MdxJsxElement extends Node {
  type: "mdxJsxFlowElement";
  name: string;
  children?: Node[];
}

interface CodeNode extends Node {
  type: "code";
  value: string;
  lang?: string;
}

function isMdxFlowElement(node: Node): node is MdxJsxElement {
  return node.type === "mdxJsxFlowElement";
}

function isElixirCodeNode(node: Node): node is CodeNode & { lang: "elixir" } {
  function isCodeNode(node: Node): node is CodeNode {
    return node.type === "code";
  }

  if (!isCodeNode(node)) return false;

  return node.lang === "elixir";
}

export function remarkCollectCode() {
  return (tree: Parent) => {
    const codeBlocks: string[] = [];

    visit<Parent, Test>(tree, (node, index, parent) => {
      const isCodeBlock = isMdxFlowElement(node) && node.name === "EditorCode";
      if (!isCodeBlock) {
        return undefined;
      }

      const canDropNodeFromParent = parent !== undefined && index !== undefined;
      if (canDropNodeFromParent) {
        parent.children.splice(index, 1);
      }

      const firstChild = (node.children ?? []).at(0);
      if (firstChild === undefined) {
        return [SKIP, index];
      }

      if (!isElixirCodeNode(firstChild)) {
        return [SKIP, index];
      }

      codeBlocks.push(firstChild.value);
      return [SKIP, index];
    });

    const combinedCode = codeBlocks.join("\n\n");

    if (codeBlocks.length > 0) {
      tree.children.unshift(createExportVariableNode(combinedCode));
    }
  };
}

function createExportVariableNode(value: string): Node {
  return {
    type: "mdxjsEsm",
    value: "",
    data: {
      estree: {
        type: "Program",
        sourceType: "module",
        body: [
          {
            type: "ExportNamedDeclaration",
            declaration: {
              type: "VariableDeclaration",
              kind: "const",
              declarations: [
                {
                  type: "VariableDeclarator",
                  id: {
                    type: "Identifier",
                    name: "defaultCode"
                  },
                  init: {
                    type: "Literal",
                    value: value,
                    raw: JSON.stringify(value)
                  }
                }
              ]
            },
            specifiers: [],
            source: null
          }
        ]
      }
    }
  } as Node;
}
