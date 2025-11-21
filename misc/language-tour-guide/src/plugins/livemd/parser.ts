import remarkParse from "remark-parse";
import { unified } from "unified";
import { visit, type Test } from "unist-util-visit";
import remarkStringify from "remark-stringify";

import type { Root, Node, Code, Html } from "mdast";

type LivebookMetadata = {
  output?: boolean;
  force_markdown?: boolean;
  [key: string]: unknown;
};

function isCodeBlock(node: Node | undefined): node is Code {
  return node?.type === "code";
}

function isCodeOutputBlock(
  node: Node | undefined
): node is Code & { lang: undefined } {
  return isCodeBlock(node) && !node.lang;
}

function isElixirCodeBlock(
  node: Node | undefined
): node is Code & { lang: "elixir" } {
  return isCodeBlock(node) && node.lang === "elixir";
}

function isHtmlNode(node: Node | undefined): node is Html {
  return node?.type === "html";
}

function remarkLivebook() {
  return (tree: Root) => {
    visit<Root, Test>(tree, (node, index, parent) => {
      if (!parent || index === undefined) return;

      // Process Elixir code blocks - convert to Editor unless force_markdown is set
      if (isElixirCodeBlock(node)) {
        // Check if there's a force_markdown metadata before this code block
        if (index > 0) {
          const prevNode = parent.children[index - 1];
          if (isHtmlNode(prevNode)) {
            const metadataMatch = prevNode.value.match(
              /<!--\s*livebook:({.*?})\s*-->/
            );

            if (metadataMatch) {
              const metadata: LivebookMetadata = JSON.parse(metadataMatch[1]);

              // If force_markdown is true, keep it as a normal code block
              if (metadata.force_markdown === true) {
                parent.children.splice(index - 1, 1);
                return;
              }
            }
          }
        }

        const initCode = wrapProps(node.value);
        let outputStd = "``";
        let outputResult = "``";
        let nodesToRemove = 0;

        // Check if there's output metadata after this code block
        const nextNode = parent.children[index + 1];
        if (isHtmlNode(nextNode)) {
          const metadataMatch = nextNode.value.match(
            /<!--\s*livebook:({.*?})\s*-->/
          );
          if (metadataMatch) {
            const metadata: LivebookMetadata = JSON.parse(metadataMatch[1]);

            if (metadata.output === true) {
              const firstOutputNode = parent.children[index + 2];

              if (!isCodeOutputBlock(firstOutputNode)) {
                throw new Error(
                  "Livemd parse error: Expected output code block after output:true metadata"
                );
              }

              outputStd = wrapProps(firstOutputNode.value);
              nodesToRemove = 2;

              // Check if there's a second output block
              const secondMetadataNode = parent.children[index + 3];
              if (isHtmlNode(secondMetadataNode)) {
                const secondMatch = secondMetadataNode.value.match(
                  /<!--\s*livebook:({.*?})\s*-->/
                );

                if (secondMatch) {
                  const secondMetadata: LivebookMetadata = JSON.parse(
                    secondMatch[1]
                  );

                  if (secondMetadata.output === true) {
                    const secondOutputNode = parent.children[index + 4];

                    if (!isCodeOutputBlock(secondOutputNode)) {
                      throw new Error(
                        "Livemd parse error: Expected second output code block"
                      );
                    }

                    outputResult = wrapProps(secondOutputNode.value);
                    nodesToRemove = 4;
                  }
                }
              }
            }
          }
        }

        const editorNode = {
          type: "html" as const,
          value: `<Editor init_code={${initCode}} output_std={${outputStd}} output_result={${outputResult}} />`
        };

        // Replace the Elixir code block with Editor component
        parent.children[index] = editorNode;

        // Remove output metadata and blocks if they exist
        if (nodesToRemove > 0) {
          parent.children.splice(index + 1, nodesToRemove);
        }
      } else if (isHtmlNode(node)) {
        // Remove any remaining livebook metadata comments
        const metadataMatch = node.value.match(/<!--\s*livebook:({.*?})\s*-->/);
        if (metadataMatch) {
          parent.children.splice(index, 1);
        }
      }
    });
  };
}

function wrapProps(value: string): string {
  const escaped = value.replace(/`/g, "\\`");
  return "`" + escaped + "`";
}

export async function transformToMdx(markdown: string) {
  const processor = unified()
    .use(remarkParse)
    .use(remarkLivebook)
    .use(remarkStringify, {
      bullet: "-",
      fence: "`",
      fences: true
    });

  return String(await processor.process(markdown));
}
