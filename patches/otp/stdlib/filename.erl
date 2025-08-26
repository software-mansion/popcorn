-module(filename).
-compile({popcorn_patch_private, filename_string_to_binary/1}).

filename_string_to_binary(List) ->
    %Patch reason: changed unicode to utf8 due to lack of support.
    case
        unicode:characters_to_binary(popcorn_module:flatten(List), utf8, file:native_name_encoding())
    of
        {error, _, _} ->
            erlang:error(badarg);
        Bin when is_binary(Bin) ->
            Bin
    end.
