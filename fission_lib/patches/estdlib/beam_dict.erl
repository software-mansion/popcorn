-module(beam_dict).
-compile({flb_patch_private, my_term_to_binary/1}).

my_term_to_binary(Term) ->
    %Patch reason: usage of unsupported options
    term_to_binary(Term).
