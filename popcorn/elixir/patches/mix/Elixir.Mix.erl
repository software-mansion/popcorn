%% Mix.install/2 can install Req, whose requests need Popcorn's adapter in the browser.
-module('Elixir.Mix').
-export([install/2]).

install(Deps, Opts) ->
    Result = popcorn_module:install(Deps, Opts),
    ok = 'Elixir.Popcorn.Fetch':install_req_adapter(),
    Result.
