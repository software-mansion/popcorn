-module(unicode_util).
-export([lookup/1, category/1, nfd/1, nfkd/1, nfc/1, nfkc/1]).

%% The Unicode character database takes a quarter of a megabyte, for looking
%% up characters' properties and normalizing. These raise undef instead, as
%% if they weren't there, except for text that normalizing leaves as it is:
%% ASCII, Latin-1 for NFC, and CRLF. Then the compiler removes the database.
%% Inspecting atoms normalizes them, see elixir/Elixir.String.Tokenizer.erl.

lookup(CP) -> erlang:error(undef, [CP]).

category(CP) -> erlang:error(undef, [CP]).

nfd(Str0) ->
    case popcorn_module:gc(Str0) of
        [GC | _] = Str when is_integer(GC), GC >= 0, GC =< 127; GC =:= "\r\n" -> Str;
        [_ | _] -> erlang:error(undef, [Str0]);
        Other -> Other
    end.

nfkd(Str0) ->
    case popcorn_module:gc(Str0) of
        [GC | _] = Str when is_integer(GC), GC >= 0, GC =< 127; GC =:= "\r\n" -> Str;
        [_ | _] -> erlang:error(undef, [Str0]);
        Other -> Other
    end.

nfc(Str0) ->
    case popcorn_module:gc(Str0) of
        [GC | _] = Str when is_integer(GC), GC >= 0, GC =< 255; GC =:= "\r\n" -> Str;
        [_ | _] -> erlang:error(undef, [Str0]);
        Other -> Other
    end.

nfkc(Str0) ->
    case popcorn_module:gc(Str0) of
        [GC | _] = Str when is_integer(GC), GC >= 0, GC =< 127; GC =:= "\r\n" -> Str;
        [_ | _] -> erlang:error(undef, [Str0]);
        Other -> Other
    end.
