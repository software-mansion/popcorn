import { Popcorn } from "@swmansion/popcorn";

window.popcornInstances = new Map();

window.createPopcornInstance = async function (bundlePath) {
  const id = crypto.randomUUID();
  const logs = [];

  const popcorn = await Popcorn.init({
    bundlePath,
    onStdout: (output) => logs.push(`[popcorn stdout] ${output}\n`),
  });

  window.popcornInstances.set(id, { popcorn, logs });
  return id;
};

window.destroyPopcornInstance = function (id) {
  const instance = window.popcornInstances.get(id);
  if (instance) {
    instance.popcorn.deinit();
    window.popcornInstances.delete(id);
  }
};
