-module(init).

-export([get_arguments/0, get_argument/1, boot2/1]).

% Patch reason: this is a mock implementation,
% sufficient for some use cases not to break
get_arguments() -> [].
get_argument(_Arg) -> error.

boot2([<<"-s">>, StartupModule]) when is_atom(StartupModule) ->
    % Until we have boot scripts, we just start kernel application.
    % {ok, _KernelPid} = kernel:start(boot, []),
    StartupModule:start().
