-module(unicode).
-export([characters_to_nfd_list/1, characters_to_nfd_binary/1,
         characters_to_nfkd_list/1, characters_to_nfkd_binary/1,
         characters_to_nfc_list/1, characters_to_nfc_binary/1,
         characters_to_nfkc_list/1, characters_to_nfkc_binary/1]).

%% The originals turn any error into badarg, including the undef of
%% unicode_util's normalizing, see unicode_util.erl. It's raised as it is
%% instead. The badarg is raised right in the function, so the stack trace
%% points to it for erl_stdlib_errors.

-define(GC_N, 200).
-define(BADARG(CD), erlang:error(badarg, [CD], [{error_info, #{module => erl_stdlib_errors}}])).

characters_to_nfd_list(CD) ->
    try popcorn_module:characters_to_nfd_list(CD, [])
    catch
        error:undef:Stacktrace -> erlang:raise(error, undef, Stacktrace);
        error:_ -> ?BADARG(CD)
    end.

characters_to_nfd_binary(CD) ->
    try popcorn_module:characters_to_nfd_binary(CD, ?GC_N, [], [])
    catch
        error:undef:Stacktrace -> erlang:raise(error, undef, Stacktrace);
        error:_ -> ?BADARG(CD)
    end.

characters_to_nfkd_list(CD) ->
    try popcorn_module:characters_to_nfkd_list(CD, [])
    catch
        error:undef:Stacktrace -> erlang:raise(error, undef, Stacktrace);
        error:_ -> ?BADARG(CD)
    end.

characters_to_nfkd_binary(CD) ->
    try popcorn_module:characters_to_nfkd_binary(CD, ?GC_N, [], [])
    catch
        error:undef:Stacktrace -> erlang:raise(error, undef, Stacktrace);
        error:_ -> ?BADARG(CD)
    end.

characters_to_nfc_list(CD) ->
    try popcorn_module:characters_to_nfc_list(CD, [])
    catch
        error:undef:Stacktrace -> erlang:raise(error, undef, Stacktrace);
        error:_ -> ?BADARG(CD)
    end.

characters_to_nfc_binary(CD) ->
    try popcorn_module:characters_to_nfc_binary(CD, ?GC_N, [], [])
    catch
        error:undef:Stacktrace -> erlang:raise(error, undef, Stacktrace);
        error:_ -> ?BADARG(CD)
    end.

characters_to_nfkc_list(CD) ->
    try popcorn_module:characters_to_nfkc_list(CD, [])
    catch
        error:undef:Stacktrace -> erlang:raise(error, undef, Stacktrace);
        error:_ -> ?BADARG(CD)
    end.

characters_to_nfkc_binary(CD) ->
    try popcorn_module:characters_to_nfkc_binary(CD, ?GC_N, [], [])
    catch
        error:undef:Stacktrace -> erlang:raise(error, undef, Stacktrace);
        error:_ -> ?BADARG(CD)
    end.
