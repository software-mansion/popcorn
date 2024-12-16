-module(filename).
-compile({flb_patch_private, unix_splitb/1}).
-compile({flb_patch_private, filename_string_to_binary/1}).

unix_splitb(Name) ->
    %Patch reason: binary:split using list of separators.
    L = binary:split(Name, <<"/">>, [global]),
    LL =
        case L of
            [<<>> | Rest] when Rest =/= [] ->
                [<<"/">> | Rest];
            _ ->
                L
        end,
    [X || X <- LL, X =/= <<>>].

filename_string_to_binary(List) ->
    %Patch reason: changed unicode to utf8 due to lack of support.
    case
        unicode:characters_to_binary(flb_module:flatten(List), utf8, file:native_name_encoding())
    of
        {error, _, _} ->
            erlang:error(badarg);
        Bin when is_binary(Bin) ->
            Bin
    end.
