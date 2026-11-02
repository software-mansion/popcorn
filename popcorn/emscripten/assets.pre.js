// Keep Emscripten's URL resolution and pthread module handoff intact.
var popcornInstantiateAsync = instantiateAsync;
instantiateAsync = function(binary, url, imports) {
  if (!binary && Module['instantiateWasmFromUrl']) {
    return Module['instantiateWasmFromUrl'](url, imports);
  }
  return popcornInstantiateAsync(binary, url, imports);
};
