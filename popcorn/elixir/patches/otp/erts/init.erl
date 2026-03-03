-module(init).

-export([get_arguments/0, get_argument/1, boot/1]).

% Patch reason: this is a mock implementation,
% sufficient for some use cases not to break
get_arguments() -> [].
get_argument(_Arg) -> error.

% Patch reason: OTP's init:boot doesn't work on AtomVM -
% it has a custom implementation in its stdlib.
% The AtomVM's implementation starts the kenel app
% and runs the startup module. We start all apps
% in Popcorn.Boot, so here we just run the StartupModule.
boot([<<"-s">>, StartupModule]) when is_atom(StartupModule) ->
    StartupModule:start().
