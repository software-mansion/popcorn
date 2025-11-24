import { defineConfig, UserConfig } from "tsdown";
import util from "util";
import { exec as execRaw } from "child_process";
const exec = util.promisify(execRaw);

const rebuildPackage: UserConfig = {
  ignoreWatch: [/swmansion-popcorn.+.tgz/],
  hooks(hooks) {
    hooks.hook("build:done", () => {
      exec("npm pack");
    });
  },
};

export default defineConfig({
  exports: true,
  ...rebuildPackage,
});
