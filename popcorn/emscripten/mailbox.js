addToLibrary({
  // WebKit can lose waitAsync wakeups: https://bugs.webkit.org/show_bug.cgi?id=325822
  $waitAsyncPolyfilled__postset: 'if (!ENVIRONMENT_IS_PTHREAD) waitAsyncPolyfilled = true;',
});
