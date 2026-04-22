import typescript from "@rollup/plugin-typescript";

export default [
  // Main library
  {
    input: "src/index.ts",
    output: {
      dir: "dist",
      format: "esm",
      entryFileNames: "[name].mjs",
      preserveModules: true,
      preserveModulesRoot: "src",
    },
    cache: false,
    plugins: [
      typescript({ tsconfig: "./tsconfig.json", outputToFilesystem: true }),
    ],
  },
];
