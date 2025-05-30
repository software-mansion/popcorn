%
% This file is part of AtomVM.
%
% Copyright 2021 Fred Dushin <fred@dushin.net>
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
%
%    http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.
%
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
%

%%-----------------------------------------------------------------------------
%% @doc A <em>naive</em> implementation of the Erlang/OTP `maps' interface.
%%
%% The `maps' module provides several convenience operations for interfacing
%% with the Erlang map type, which associates (unique) keys with values.
%%
%% Note that the ordering of entries in a map is implementation-defined.  While
%% many operations in this module present entries in lexical order, users should
%% in general make no assumptions about the ordering of entries in a map.
%%
%% This module implements a subset of the Erlang/OTP `maps' interface.
%% Some OTP functions are not implemented, and the approach favors
%% correctness and readability over speed and performance.
%% @end
%%-----------------------------------------------------------------------------
-module(maps).

-export([
    get/2, get/3,
    is_key/2,
    put/3,
    iterator/1,
    iterator/2,
    next/1,
    new/0,
    keys/1,
    values/1,
    to_list/1,
    from_list/1,
    size/1,
    find/2,
    filter/2,
    fold/3,
    foreach/2,
    from_keys/2,
    map/2,
    merge/2,
    merge_with/3,
    remove/2,
    take/2,
    update/3
]).

-export_type([
    iterator/2,
    iterator/0,
    iterator_order/1,
    iterator_order/0
]).

-opaque iterator(Key, Value) ::
    {Key, Value, iterator()}
    | none
    | nonempty_improper_list(non_neg_integer(), #{Key => Value})
    | nonempty_improper_list(list(Key), #{Key => Value}).
-type iterator() :: iterator(Key :: any(), Value :: any()).
-type iterator_order(Key) :: undefined | ordered | reversed | fun((Key, Key) -> boolean()).
-type iterator_order() :: iterator_order(Key :: term()).
-type map_or_iterator(Key, Value) :: #{Key => Value} | iterator(Key, Value).

%%-----------------------------------------------------------------------------
%% @param   Key     the key to get
%% @param   Map     the map from which to get the value
%% @returns the value in `Map' associated with `Key', if it exists.
%% @doc     Get the value in `Map' associated with `Key', if it exists.
%%
%% This function raises a `{badkey, Key}' error if 'Key' does not occur in
%% `Map' or a `{badmap, Map}' error if `Map' is not a map.
%% @end
%%-----------------------------------------------------------------------------
-spec get(Key, Map :: #{Key => Value}) -> Value.
get(Key, Map) ->
    erlang:map_get(Key, Map).

%%-----------------------------------------------------------------------------
%% @param   Key     the key
%% @param   Map     the map
%% @param   Default default value
%% @returns the value in `Map' associated with `Key', or `Default', if
%%          the key is not associated with a value in `Map'.
%% @doc     Get the value in `Map' associated with `Key', or `Default', if
%%          the key is not associated with a value in `Map'.
%%
%% This function raises a `{badmap, Map}' error if `Map' is not a map.
%% @end
%%-----------------------------------------------------------------------------
-spec get(Key, Map :: #{Key => Value}, Default :: Value) -> Value.
get(Key, Map, Default) ->
    try
        ?MODULE:get(Key, Map)
    catch
        error:{badkey, _} ->
            Default
    end.

%%-----------------------------------------------------------------------------
%% @param   Key     the key
%% @param   Map     the map
%% @returns `true' if `Key' is associated with a value in `Map'; `false', otherwise.
%% @doc     Return `true' if `Key' is associated with a value in `Map'; `false', otherwise.
%%
%% This function raises a `{badmap, Map}' error if `Map' is not a map.
%% @end
%%-----------------------------------------------------------------------------
-spec is_key(Key, Map :: #{Key => _Value}) -> boolean().
is_key(Key, Map) ->
    erlang:is_map_key(Key, Map).

%%-----------------------------------------------------------------------------
%% @param   Key     the key
%% @param   Value   the value
%% @param   Map     the map
%% @returns A copy of `Map' containing the `{Key, Value}' association.
%% @doc     Return the map containing the `{Key, Value}' association.
%%
%% If `Key' occurs in `Map' then it will be over-written.  Otherwise, the
%% returned map will contain the new association.
%%
%% This function raises a `{badmap, Map}' error if `Map' is not a map.
%% @end
%%-----------------------------------------------------------------------------
-spec put(Key, Value, Map :: #{Key => Value}) -> #{Key => Value}.
put(Key, Value, Map) when is_map(Map) ->
    Map#{Key => Value};
put(_Key, _Value, Map) when not is_map(Map) ->
    error({badmap, Map}).

%% @equiv iterator(Map, undefined)
-spec iterator(Map :: #{Key => Value}) -> iterator(Key, Value).
iterator(Map) ->
    iterator(Map, undefined).

%%-----------------------------------------------------------------------------
%% @param   Map     the map
%% @param   Order   the iterator order, or undefined for default (unspecified)
%% order.
%% @returns an iterator structure that can be used to iterate over associations
%% in a map.
%% @see next/1
%% @doc Return an iterator structure that can be used to iterate over associations
%% in a map.
%%
%% This function raises a `{badmap, Map}' error if `Map' is not a map.
%% @end
%%-----------------------------------------------------------------------------
-spec iterator(Map :: #{Key => Value}, Order :: iterator_order()) -> iterator(Key, Value).
iterator(Map, undefined) when is_map(Map) ->
    [0 | Map];
iterator(Map, Order) when is_map(Map) ->
    Keys = iterate_keys(maps:next(maps:iterator(Map)), Order, []),
    [Keys | Map];
iterator(Map, _Order) ->
    error({badmap, Map}).

%%-----------------------------------------------------------------------------
%% @param   Iterator a map iterator
%% @returns the key and value, along with the next iterator in the map, or the
%%          atom `none' if there are no more items over which to iterate.
%% @doc Returns the next key and value in the map, along with
%% a new iterator that can be used to iterate over the remainder of the map.
%%
%% This function raises a `badarg' error if the supplied iterator is not
%% of the expected type.  Only use iterators that are returned from functions
%% in this module.
%% @end
%%-----------------------------------------------------------------------------
-spec next(Iterator :: iterator(Key, Value)) ->
    {Key, Value, iterator(Key, Value)} | none.
next([_Pos | _Map] = _Iterator) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @returns a new map
%% @doc Return a new (empty) map.
%% @end
%%-----------------------------------------------------------------------------
-spec new() -> map().
new() ->
    #{}.

%%-----------------------------------------------------------------------------
%% @param   Map     the map
%% @returns the list of keys that occur in this map.
%% @doc Returns the list of keys that occur in this map.
%%
%% No guarantees are provided about the order of keys returned from this function.
%%
%% This function raises a `{badmap, Map}' error if `Map' is not a map.
%% @end
%%-----------------------------------------------------------------------------
-spec keys(Map :: #{Key => _Value}) -> [Key].
keys(Map) when is_map(Map) ->
    iterate_keys(maps:next(maps:iterator(Map)), undefined, []);
keys(Map) ->
    error({badmap, Map}).

%%-----------------------------------------------------------------------------
%% @param   Map     the map
%% @returns the list of values that occur in this map.
%% @doc Returns the list of values that occur in this map.
%%
%% No guarantees are provided about the order of values returned from this function.
%%
%% This function raises a `{badmap, Map}' error if `Map' is not a map.
%% @end
%%-----------------------------------------------------------------------------
-spec values(Map :: #{_Key => Value}) -> [Value].
values(Map) when is_map(Map) ->
    iterate_values(maps:next(maps:iterator(Map)), []);
values(Map) ->
    error({badmap, Map}).

%%-----------------------------------------------------------------------------
%% @param   MapOrIterator     the map or iterator
%% @returns a list of `[{Key, Value}]' tuples
%% @doc Return the list of entries, expressed as `{Key, Value}' pairs, in the supplied map.
%%
%% If provided with a map, no guarantees are provided about the order of
%% entries returned from this function. Order can be controlled with `iterator/2'
%%
%% This function raises a `{badmap, Map}' error if `Map' is not a map and not
%% an iterator.
%% @end
%%-----------------------------------------------------------------------------
-spec to_list(Map :: #{Key => Value}) -> [{Key, Value}].
to_list(Map) when is_map(Map) ->
    to_list(maps:iterator(Map));
to_list(Iterator) when is_list(Iterator) andalso is_map(tl(Iterator)) ->
    iterate_entries(maps:next(Iterator), []);
to_list(Map) ->
    error({badmap, Map}).

%%-----------------------------------------------------------------------------
%% @param   List a list of `[{Key, Value}]' pairs
%% @returns the map containing the entries from the list of supplied key-value pairs.
%% @doc This function constructs a map from the supplied list of key-value pairs.
%%
%% If the input list contains duplicate keys, the returned map will contain the
%% right-most entry.
%%
%% This function will raise a `badarg' error if the input is not a proper
%% list or contains an element that is not a key-value pair.
%% @end
%%-----------------------------------------------------------------------------
-spec from_list(List :: [{Key, Value}]) -> #{Key => Value}.
from_list(List) when is_list(List) ->
    iterate_from_list(List, ?MODULE:new());
from_list(_List) ->
    error(badarg).

%%-----------------------------------------------------------------------------
%% @param   Map the map
%% @returns the size of the map
%% @doc Returns the size of (i.e., the number of entries in) the map
%%
%% This function raises a `{badmap, Map}' error if `Map' is not a map.
%% @end
%%-----------------------------------------------------------------------------
-spec size(Map :: map()) -> non_neg_integer().
size(Map) when is_map(Map) ->
    erlang:map_size(Map);
size(Map) ->
    error({badmap, Map}).

%%-----------------------------------------------------------------------------
%% @param   Key     the key to find
%% @param   Map     the map in which to search
%% @returns `{ok, Value}' if `Key' is in `Map'; `error', otherwise.
%% @doc Returns `{ok, Value}' if `Key' is in `Map'; `error', otherwise.
%%
%% This function raises a `{badmap, Map}' error if `Map' is not a map.
%% @end
%%-----------------------------------------------------------------------------
-spec find(Key, Map :: #{Key => Value}) -> {ok, Value} | error.
find(Key, Map) ->
    try
        {ok, ?MODULE:get(Key, Map)}
    catch
        _:{badkey, _} ->
            error
    end.

%%-----------------------------------------------------------------------------
%% @param   Pred    a function used to filter entries from the map
%% @param   MapOrIterator the map or map iterator to filter
%% @returns a map containing all elements in `MapOrIterator' that satisfy `Pred'
%% @doc Return a map who's entries are filtered by the supplied predicate.
%%
%% This function returns a new map containing all elements from the input
%% `MapOrIterator' that satisfy the input `Pred'.
%%
%% The supplied predicate is a function from key-value inputs to a boolean value.
%%
%% This function raises a `{badmap, Map}' error if `Map' is not a map or map
%% iterator, and a `badarg' error if the input predicate is not a function.
%% @end
%%-----------------------------------------------------------------------------
-spec filter(
    Pred :: fun((Key, Value) -> boolean()),
    MapOrIterator :: map_or_iterator(Key, Value)
) -> #{Key => Value}.
filter(Pred, Map) when is_function(Pred, 2) andalso is_map(Map) ->
    iterate_filter(Pred, maps:next(maps:iterator(Map)), ?MODULE:new());
filter(Pred, [Pos | Map] = Iterator) when
    is_function(Pred, 2) andalso is_integer(Pos) andalso is_map(Map)
->
    iterate_filter(Pred, maps:next(Iterator), ?MODULE:new());
filter(_Pred, Map) when not is_map(Map) ->
    error({badmap, Map});
filter(_Pred, _Map) ->
    error(badarg).

%%-----------------------------------------------------------------------------
%% @param   Fun     function over which to fold values
%% @param   Init    the initial value of the fold accumulator
%% @param   MapOrIterator the map or map iterator over which to fold
%% @returns the result of folding over all elements of the supplied map.
%% @doc Fold over the entries in a map.
%%
%% This function takes a function used to fold over all entries in a map
%% and an initial accumulator value to use as the value supplied to the
%% first entry in the map.
%%
%% This function raises a `badmap' error if `Map' is not a map or map iterator,
%% and a `badarg' error if the input function is not a function.
%% @end
%%-----------------------------------------------------------------------------
-spec fold(
    Fun :: fun((Key, Value, Accum) -> Accum),
    Accum,
    MapOrIterator :: map_or_iterator(Key, Value)
) -> Accum.
fold(Fun, Init, Map) when is_function(Fun, 3) andalso is_map(Map) ->
    iterate_fold(Fun, maps:next(maps:iterator(Map)), Init);
fold(Fun, Init, [Pos | Map] = Iterator) when
    is_function(Fun, 3) andalso is_integer(Pos) andalso is_map(Map)
->
    iterate_fold(Fun, maps:next(Iterator), Init);
fold(_Fun, _Init, Map) when not is_map(Map) ->
    error({badmap, Map});
fold(_Fun, _Init, _Map) ->
    error(badarg).

%%-----------------------------------------------------------------------------
%% @param   Fun     function to call with every key-value pair
%% @param   MapOrIterator the map or map iterator over which to iterate
%% @returns `ok'
%% @doc Iterate over the entries in a map.
%%
%% This function takes a function used to iterate over all entries in a map.
%%
%% This function raises a `badmap' error if `Map' is not a map or map iterator,
%% and a `badarg' error if the input function is not a function.
%% @end
%%-----------------------------------------------------------------------------
-spec foreach(
    Fun :: fun((Key, Value) -> any()),
    MapOrIterator :: map_or_iterator(Key, Value)
) -> ok.
foreach(Fun, Map) when is_function(Fun, 2) andalso is_map(Map) ->
    iterate_foreach(Fun, maps:next(maps:iterator(Map)));
foreach(Fun, [Pos | Map] = Iterator) when
    is_function(Fun, 2) andalso is_integer(Pos) andalso is_map(Map)
->
    iterate_foreach(Fun, maps:next(Iterator));
foreach(_Fun, Map) when not is_map(Map) ->
    error({badmap, Map});
foreach(_Fun, _Map) ->
    error(badarg).

%%-----------------------------------------------------------------------------
%% @param   List    the list of keys of the map that will be created
%% @param   Value   the value that will be used as value for all map items
%% @returns a map having all provided keys having provided value as value
%% @doc Creates a map with specified keys intialized to given value
%% @end
%%-----------------------------------------------------------------------------
-spec from_keys(list(), term()) -> map().
from_keys(List, _Value) when is_list(List) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Fun     the function to apply to every entry in the map
%% @param   Map     the map to which to apply the map function
%% @returns the result of applying `Fun' to every entry in `Map'
%% @doc Returns the result of applying a function to every element of a map.
%%
%% This function raises a `badmap' error if `Map' is not a map or map iterator,
%% and a `badarg' error if the input function is not a function.
%% @end
%%-----------------------------------------------------------------------------
-spec map(Fun :: fun((Key, Value) -> MappedValue), Map :: map_or_iterator(Key, Value)) ->
    #{Key => MappedValue}.
map(Fun, Map) when is_function(Fun, 2) andalso is_map(Map) ->
    iterate_map(Fun, maps:next(maps:iterator(Map)), ?MODULE:new());
map(Fun, [Pos | Map] = Iterator) when
    is_function(Fun, 2) andalso is_integer(Pos) andalso is_map(Map)
->
    iterate_map(Fun, maps:next(Iterator), ?MODULE:new());
map(_Fun, Map) when not is_map(Map) ->
    error({badmap, Map});
map(_Fun, _Map) ->
    error(badarg).

%%-----------------------------------------------------------------------------
%% @param   Map1  a map
%% @param   Map2  a map
%% @returns the result of merging entries from `Map1' and `Map2'.
%% @doc Merge two maps to yield a new map.
%%
%% If `Map1' and `Map2' contain the same key, then the value from `Map2' will be used.
%%
%% This function raises a `badmap' error if neither `Map1' nor `Map2' is a map.
%% @end
%%-----------------------------------------------------------------------------
-spec merge(Map1 :: #{Key => Value}, Map2 :: #{Key => Value}) -> #{Key => Value}.
merge(Map1, Map2) when is_map(Map1) andalso is_map(Map2) ->
    iterate_merge(maps:next(maps:iterator(Map2)), Map1);
merge(Map1, _Map2) when not is_map(Map1) ->
    error({badmap, Map1});
merge(_Map1, Map2) when not is_map(Map2) ->
    error({badmap, Map2}).

%%-----------------------------------------------------------------------------
%% @param   Combiner  a function to merge values from Map1 and Map2 if a key exists in both maps
%% @param   Map1  a map
%% @param   Map2  a map
%% @returns the result of merging entries from `Map1' and `Map2'.
%% @doc Merge two maps to yield a new map.
%%
%% If `Map1' and `Map2' contain the same key, then the value from `Combiner(Key, Value1, Value2)' will be used.
%%
%% This function raises a `badmap' error if neither `Map1' nor `Map2' is a map.
%% @end
%%-----------------------------------------------------------------------------
-spec merge_with(
    Combiner :: fun((Key, Value, Value) -> Value), Map1 :: #{Key => Value}, Map2 :: #{Key => Value}
) -> #{Key => Value}.
merge_with(Combiner, Map1, Map2) when is_map(Map1) andalso is_map(Map2) ->
    iterate_merge_with(Combiner, maps:next(maps:iterator(Map1)), Map2);
merge_with(_Combiner, Map1, _Map2) when not is_map(Map1) ->
    error({badmap, Map1});
merge_with(_Combiner, _Map1, Map2) when not is_map(Map2) ->
    error({badmap, Map2}).

%%-----------------------------------------------------------------------------
%% @param   Key     the key to remove
%% @param   MapOrIterator     the map or map iterator from which to remove the key
%% @returns a new map without `Key' as an entry.
%% @doc Remove an entry from a map using a key.
%%
%% If `Key' does not occur in `Map', then the returned Map has the same
%% entries as the input map or map iterator.
%%
%% Note.  This function extends the functionality of the OTP `remove/2' function,
%% since the OTP interface only takes a map as input.
%%
%% This function raises a `badmap' error if `Map' is not a map or map iterator.
%% @end
%%-----------------------------------------------------------------------------
-spec remove(Key, MapOrIterator :: map_or_iterator(Key, Value)) -> #{Key => Value}.
remove(Key, Map) when is_map(Map) ->
    case ?MODULE:is_key(Key, Map) of
        true ->
            iterate_remove(Key, maps:next(maps:iterator(Map)), ?MODULE:new());
        _ ->
            Map
    end;
remove(Key, [Pos | Map] = Iterator) when is_integer(Pos) andalso is_map(Map) ->
    iterate_remove(Key, maps:next(Iterator), ?MODULE:new());
remove(_Key, Map) when not is_map(Map) ->
    error({badmap, Map}).

take(Key, Map) when is_map(Map) ->
    case ?MODULE:is_key(Key, Map) of
        true ->
            {_K, V, _It} = Next = maps:next(maps:iterator(Map)),
            Map2 = iterate_remove(Key, Next, ?MODULE:new()),
            {V, Map2};
        _ ->
            Map
    end;
take(_Key, Map) ->
    error({badmap, Map}).

%%-----------------------------------------------------------------------------
%% @param   Key     the key to update
%% @param   Value   the value to update
%% @param   Map     the map to update
%% @returns a new map, with `Key' updated with `Value'
%% @doc Returns a new map with an updated key-value association.
%%
%% This function raises a `badmap' error if `Map' is not a map and
%% `{badkey, Key}` if key doesn't exist
%% @end
%%-----------------------------------------------------------------------------
-spec update(Key, Value, Map :: #{Key => Value}) -> #{Key => Value}.
update(Key, Value, Map) ->
    _ = ?MODULE:get(Key, Map),
    Map#{Key => Value}.

%%
%% Internal functions
%%

%% @private
iterate_keys(none, undefined, Accum) ->
    lists:reverse(Accum);
iterate_keys(none, ordered, Accum) ->
    lists:sort(Accum);
iterate_keys(none, reversed, Accum) ->
    lists:reverse(lists:sort(Accum));
iterate_keys(none, F, Accum) ->
    lists:sort(F, Accum);
iterate_keys({Key, _Value, Iterator}, Order, Accum) ->
    iterate_keys(maps:next(Iterator), Order, [Key | Accum]).

%% @private
iterate_values(none, Accum) ->
    lists:reverse(Accum);
iterate_values({_Key, Value, Iterator}, Accum) ->
    iterate_values(maps:next(Iterator), [Value | Accum]).

%% @private
iterate_entries(none, Accum) ->
    lists:reverse(Accum);
iterate_entries({Key, Value, Iterator}, Accum) ->
    iterate_entries(maps:next(Iterator), [{Key, Value} | Accum]).

%% @private
iterate_filter(_Pred, none, Accum) ->
    Accum;
iterate_filter(Pred, {Key, Value, Iterator}, Accum) ->
    NewAccum =
        case Pred(Key, Value) of
            true ->
                Accum#{Key => Value};
            _ ->
                Accum
        end,
    iterate_filter(Pred, maps:next(Iterator), NewAccum).

%% @private
iterate_fold(_Fun, none, Accum) ->
    Accum;
iterate_fold(Fun, {Key, Value, Iterator}, Accum) ->
    NewAccum = Fun(Key, Value, Accum),
    iterate_fold(Fun, maps:next(Iterator), NewAccum).

%% @private
iterate_foreach(_Fun, none) ->
    ok;
iterate_foreach(Fun, {Key, Value, Iterator}) ->
    _ = Fun(Key, Value),
    iterate_foreach(Fun, maps:next(Iterator)).

%% @private
iterate_map(_Fun, none, Accum) ->
    Accum;
iterate_map(Fun, {Key, Value, Iterator}, Accum) ->
    NewAccum = Accum#{Key => Fun(Key, Value)},
    iterate_map(Fun, maps:next(Iterator), NewAccum).

%% @private
iterate_merge_with(_Combiner, none, Accum) ->
    Accum;
iterate_merge_with(Combiner, {Key, Value1, Iterator}, Accum) ->
    case Accum of
        #{Key := Value2} ->
            iterate_merge_with(Combiner, maps:next(Iterator), Accum#{
                Key := Combiner(Key, Value1, Value2)
            });
        #{} ->
            iterate_merge_with(Combiner, maps:next(Iterator), Accum#{Key => Value1})
    end.

%% @private
iterate_merge(none, Accum) ->
    Accum;
iterate_merge({Key, Value, Iterator}, Accum) ->
    iterate_merge(maps:next(Iterator), Accum#{Key => Value}).

%% @private
iterate_remove(_Key, none, Accum) ->
    Accum;
iterate_remove(Key, {Key, _Value, Iterator}, Accum) ->
    iterate_remove(Key, maps:next(Iterator), Accum);
iterate_remove(Key, {OtherKey, Value, Iterator}, Accum) ->
    iterate_remove(Key, maps:next(Iterator), Accum#{OtherKey => Value}).

%% @private
iterate_from_list([], Accum) ->
    Accum;
iterate_from_list([{Key, Value} | T], Accum) ->
    iterate_from_list(T, Accum#{Key => Value});
iterate_from_list(_List, _Accum) ->
    error(badarg).
