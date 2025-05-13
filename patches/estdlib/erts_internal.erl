-module(erts_internal).

-export([map_next/3]).

%% Patch reason: missing NIF in Atom
%%
%% return the next assoc in the iterator and a new iterator
-spec map_next(I, M, A) -> {K, V, NI} | list() when
    I :: non_neg_integer() | list(),
    M :: map(),
    K :: term(),
    V :: term(),
    A :: iterator | list(),
    NI :: maps:iterator().

%% Patch reason: it uses native code in OTP
map_next(I, M, iterator) ->
    maps:next([I | M]).
