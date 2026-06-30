import type { ThemeRegistration } from "shiki";

export const popcornTheme: ThemeRegistration = {
  name: "popcorn-dark",
  type: "dark",
  colors: {
    "editor.background": "var(--color-llv-bg-100)",
    "editor.foreground": "var(--color-llv-fg-muted)",
  },
  tokenColors: [
    {
      scope: [
        "keyword.control",
        "keyword.other",
        "storage.type",
        "storage.modifier",
        "keyword.operator.macro",
      ],
      settings: { foreground: "var(--color-orange-100)" },
    },
    {
      scope: [
        "entity.name.type",
        "entity.name.class",
        "support.class",
        "entity.name.namespace",
      ],
      settings: { foreground: "var(--color-llv-orange-60)", fontStyle: "bold" },
    },
    {
      scope: [
        "entity.name.function",
        "support.function",
        "support.function.kernel",
      ],
      settings: { foreground: "var(--color-llv-orange-40)" },
    },
    {
      scope: [
        "constant.language.symbol",
        "string.other.symbol",
        "constant.other.symbol",
      ],
      settings: { foreground: "var(--color-llv-orange-50)" },
    },
    {
      scope: [
        "punctuation.section.embedded",
        "variable.other.readwrite.elixir",
      ],
      settings: { foreground: "var(--color-llv-orange-70)", fontStyle: "bold" },
    },
    {
      scope: ["string.quoted", "string.interpolated"],
      settings: { foreground: "var(--color-llv-green-60)" },
    },
    {
      scope: ["constant.numeric"],
      settings: { foreground: "var(--color-llv-orange-60)" },
    },
    {
      scope: ["keyword.operator"],
      settings: { foreground: "var(--color-llv-brown-75)" },
    },
    {
      scope: ["entity.name.tag", "meta.tag"],
      settings: { foreground: "var(--color-llv-blue-60)" },
    },
    {
      scope: ["entity.other.attribute-name"],
      settings: { foreground: "var(--color-llv-blue-50)" },
    },
    {
      scope: [
        "comment",
        "keyword.operator.macro.sigil",
        "string.unquoted.heredoc",
        "punctuation.definition.string.heredoc",
      ],
      settings: { foreground: "var(--color-llv-brown-85)" },
    },
  ],
};
