-module(os).

-export([system_time/1, type/0]).

% Patch reason: NIF not available in AtomVM
system_time(Unit) -> erlang:system_time(Unit).

type() -> {unix, darwin}.
