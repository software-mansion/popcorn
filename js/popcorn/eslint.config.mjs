// @ts-check

import js from "@eslint/js";
import globals from "globals";
import tseslint from "typescript-eslint";
import { defineConfig, globalIgnores } from "eslint/config";

const dir = import.meta.dirname;

export default defineConfig([
  globalIgnores(["dist", "node_modules"]),
  {
    files: ["src/*.ts", "plugins/*.ts"],
    extends: [
      js.configs.recommended,
      tseslint.configs.strict,
      tseslint.configs.stylistic,
    ],
    languageOptions: {
      ecmaVersion: 2020,
      globals: globals.browser,
      parserOptions: {
        tsconfigRootDir: dir,
      },
    },
    rules: {
      "@typescript-eslint/consistent-type-definitions": ["error", "type"],
    },
  },
]);
