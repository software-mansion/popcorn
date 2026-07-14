import js from "@eslint/js";
import { defineConfig } from "eslint/config";
import tseslint from "typescript-eslint";
import prettier from "eslint-config-prettier";

export default defineConfig([
  {
    files: ["assets/**/*.ts"],
    extends: [js.configs.recommended, tseslint.configs.recommended, prettier],
    languageOptions: {
      parser: tseslint.parser,
      parserOptions: {
        project: "./tsconfig.json",
      },
    },
  },
  {
    ignores: ["priv/", "node_modules/"],
  },
]);
