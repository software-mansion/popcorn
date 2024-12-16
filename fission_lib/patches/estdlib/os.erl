-module(os).
-export([type/0]).

-spec type() -> {Osfamily, Osname} when
    Osfamily :: unix | win32,
    Osname :: atom().

type() ->
    %Patch reason: unsupported erlang:system_info(os_type) call.
    {unix, wasm}.
