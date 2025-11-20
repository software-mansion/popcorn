import {
  Heading1,
  Heading2,
  Heading3,
  OrderedList,
  Paragraph,
  UnorderedList,
  ListItem,
  InlineCode,
  BlockQuote,
  CodeBlock,
  Link,
  Editor
} from "./MarkdownComponents";

export const components = {
  h1: Heading1,
  h2: Heading2,
  h3: Heading3,
  p: Paragraph,
  ul: UnorderedList,
  ol: OrderedList,
  li: ListItem,
  code: InlineCode,
  blockquote: BlockQuote,
  pre: CodeBlock,
  a: Link,
  Editor
};
