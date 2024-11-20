-module(erts_internal).

-export([map_next/3]).

%% return the next assoc in the iterator and a new iterator
-spec map_next(I, M, A) -> {K, V, NI} | list() when
    I :: non_neg_integer() | list(),
    M :: map(),
    K :: term(),
    V :: term(),
    A :: iterator | list(),
    NI :: maps:iterator().

map_next(I, M, iterator) ->
    Ks = maps:keys(M),
    K = lists:nth(I + 1, Ks),
    V = maps:get(K, M),
    {K, V, [I + 1 | M]}.
