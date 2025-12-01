import { defineConfig, UserConfig } from "tsdown";
import util from "util";
import { exec as execRaw } from "child_process";
import copy from "rollup-plugin-copy";
const exec = util.promisify(execRaw);

export default defineConfig({
  exports: true,
  plugins: [
    copy({ targets: [{ src: "./assets/AtomVM.wasm", dest: "dist/" }] }),
  ],
  ...rebuildPackage(),
});

function rebuildPackage(): UserConfig {
  return {
    ignoreWatch: [/swmansion-popcorn.+.tgz/],
    hooks(hooks) {
      hooks.hook("build:done", () => {
        exec("npm pack");
      });
    },
  };
}
