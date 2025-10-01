import { visit, SKIP } from "unist-util-visit";
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

export function remarkCollectCode() {
  return (tree: Parent) => {
    const codeBlocks: string[] = [];

    visit(tree, (node: Node, index, parent) => {
      if (
        node.type === "mdxJsxFlowElement" &&
        (node as MdxJsxElement).name === "EditorCode"
      ) {
        if (parent !== undefined && parent && index !== undefined) {
          (parent as Parent).children.splice(index, 1);
        }

        const codeBlockNode = node as MdxJsxElement;

        if (!codeBlockNode.children || codeBlockNode.children?.length === 0) {
          return [SKIP, index];
        }

        const code = codeBlockNode.children[0] as CodeNode;

        if (!code.lang || code.lang !== "elixir") {
          return [SKIP, index];
        }

        codeBlocks.push(code.value);
        return [SKIP, index];
      }
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
