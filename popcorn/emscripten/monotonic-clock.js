addToLibrary({
  // Safari recalculates timeOrigin on each read, introducing clock jitter.
  $popcornTimeOrigin: 'performance.timeOrigin',
  emscripten_get_now__deps: ['$popcornTimeOrigin'],
  emscripten_get_now: function() {
    return popcornTimeOrigin + performance.now();
  },
});
