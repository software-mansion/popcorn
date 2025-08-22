import { visit } from "unist-util-visit";

export function rehypeRawCode() {
  return (tree: any) => {
    visit(tree, "element", (node) => {
      if (node.tagName === "pre" && node.children?.[0]?.tagName === "code") {
        const codeNode = node.children[0];

        // Extract the raw text from the code node
        let rawText = "";
        if (codeNode.children?.[0]?.type === "text") {
          rawText = codeNode.children[0].value;
        }
        node.properties = node.properties || {};
        node.properties.rawCode = rawText;
      }
    });
  };
}
