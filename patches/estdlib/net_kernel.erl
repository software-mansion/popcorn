-module(net_kernel).

-export([dflag_unicode_io/1]).

-spec dflag_unicode_io(pid()) -> boolean().

dflag_unicode_io(_) ->
    true.
