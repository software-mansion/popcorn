%% Mix.install/2 can install Req, whose requests need Popcorn's adapter in the
%% browser: Req's default one opens TCP connections. Popcorn installs it at boot
%% when the package has Req, and here once Mix.install/2 has started Req.
-module('Elixir.Mix').
-export([install/2]).

install(Deps, Opts) ->
    Result = popcorn_module:install(Deps, Opts),
    ok = 'Elixir.Popcorn.Fetch':install_req_adapter(),
    Result.
