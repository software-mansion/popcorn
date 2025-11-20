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

      // TODO: remove first elixir configuration code block if force_markdown is not set

      if (node.type === "html") {
        // Extract livebook metadata from HTML comments
        const livebookMatch = node.value.match(/<!--\s*livebook:({.*?})\s*-->/);
        if (!livebookMatch) return;

        const metadata: LivebookMetadata = JSON.parse(livebookMatch[1]);

        // Case 1: output: true - group previous Elixir code with next output block(s)
        if (metadata.output === true && index > 0) {
          const prevNode = parent.children[index - 1] as Code;

          // Only process if previous node is an Elixir code block
          if (!isElixirCodeBlock(prevNode)) {
            throw new Error(
              "Livemd parse error: Expected previous node to be an Elixir code block"
            );
          }

          // Find the first output block
          const firstOutputNode = parent.children[index + 1] as Code;

          if (!isCodeOutputBlock(firstOutputNode)) {
            throw new Error(
              "Livemd parse error: Expected next node to be an output code block"
            );
          }

          const initCode = wrapProps(prevNode.value);
          const outputStd = wrapProps(firstOutputNode.value);
          let outputResult = "``";

          // Check if there's a second metadata comment followed by another output block
          const secondMetadataNode = parent.children[index + 2];

          if (isHtmlNode(secondMetadataNode)) {
            const secondMatch = secondMetadataNode.value.match(
              /<!--\s*livebook:({.*?})\s*-->/
            );

            if (secondMatch) {
              const secondMetadata: LivebookMetadata = JSON.parse(
                secondMatch[1]
              );

              if (secondMetadata.output === true) {
                const secondOutputNode = parent.children[index + 3] as Code;

                if (!isCodeOutputBlock(secondOutputNode)) {
                  throw new Error(
                    "Livemd parse error: Expected second output node to be an output code block"
                  );
                }

                outputResult = wrapProps(secondOutputNode.value);

                // Remove second metadata and output block
                parent.children.splice(index + 2, 2);
              }
            }
          }

          const editorNode = {
            type: "html" as const,
            value: `<Editor init_code={${initCode}} output_std={${outputStd}} output_result={${outputResult}} />`
          };

          // Replace the Elixir code block with Editor component
          parent.children[index - 1] = editorNode;

          // Remove first metadata and output block
          parent.children.splice(index, 2);
        } else {
          // Other cases: when metadata is other than output: true

          // Just remove the metadata html comment
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
