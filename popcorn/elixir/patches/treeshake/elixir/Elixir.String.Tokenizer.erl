-module('Elixir.String.Tokenizer').
-export([validate/2]).
-compile([{popcorn_patch_private, [{validate, 2}]}]).

%% Non-ASCII identifiers are compared with their NFC normalization, which
%% raises undef beyond Latin-1, see stdlib/unicode_util.erl. They're taken as
%% not normalized then, so inspecting such atoms quotes them, like :"żółw",
%% instead of raising.
validate(Result, Kind) ->
    try popcorn_module:validate(Result, Kind)
    catch
        error:undef ->
            {Acc, Rest, Length, false, _Scriptset, Special} = Result,
            {Kind, lists:reverse(Acc), Rest, Length, false,
             [nfkc | lists:delete(nfkc, Special)]}
    end.
