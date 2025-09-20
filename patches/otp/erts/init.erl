-module(init).

-export([get_arguments/0, get_argument/1, boot/1]).

% Patch reason: this is a mock implementation,
% sufficient for some use cases not to break
get_arguments() -> [].
get_argument(_Arg) -> error.

boot([<<"-s">>, StartupModule]) when is_atom(StartupModule) ->
    StartupModule:start().
