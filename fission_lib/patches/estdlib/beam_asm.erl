-module(beam_asm).

-compile({flb_patch_private, encode_arg/2}).

-include("beam_opcodes.hrl").
-include("beam_asm.hrl").

encode_arg(#tr{r = {x, X}, t = Type}, Dict0) when is_integer(X), X >= 0 ->
    %% Gracefully prevent this module from being loaded in OTP 24 and below by
    %% forcing an opcode it doesn't understand. It would of course fail to load
    %% without this, but the error message wouldn't be very helpful.
    Canary = beam_opcodes:opcode(call_fun2, 3),
    {Index, Dict} = beam_dict:type(Type, beam_dict:opcode(Canary, Dict0)),
    Data =
        [flb_module:encode(?tag_z, 5),
         flb_module:encode(?tag_x, X),
         flb_module:encode(?tag_u, Index)],
    {Data, Dict};
encode_arg(#tr{r = {y, Y}, t = Type}, Dict0) when is_integer(Y), Y >= 0 ->
    Canary = beam_opcodes:opcode(call_fun2, 3),
    {Index, Dict} = beam_dict:type(Type, beam_dict:opcode(Canary, Dict0)),
    Data =
        [flb_module:encode(?tag_z, 5),
         flb_module:encode(?tag_y, Y),
         flb_module:encode(?tag_u, Index)],
    {Data, Dict};
encode_arg({x, X}, Dict) when is_integer(X), X >= 0 ->
    {flb_module:encode(?tag_x, X), Dict};
encode_arg({y, Y}, Dict) when is_integer(Y), Y >= 0 ->
    {flb_module:encode(?tag_y, Y), Dict};
encode_arg({atom, Atom}, Dict0) when is_atom(Atom) ->
    {Index, Dict} = beam_dict:atom(Atom, Dict0),
    {flb_module:encode(?tag_a, Index), Dict};
encode_arg({integer, N}, Dict) when N > -(1 bsl 32) andalso N < 1 bsl 32 ->
    %% Conservatively assume that all integers whose absolute
    %% value is greater than 1 bsl 128 will be bignums in
    %% the runtime system.
    %% Patch reason: added guard for integer being in range of 32-bit signed
    %integer due to AVM not supporting bignums
    {flb_module:encode(?tag_i, N), Dict};
encode_arg(nil, Dict) ->
    {flb_module:encode(?tag_a, 0), Dict};
encode_arg({f, W}, Dict) ->
    {flb_module:encode(?tag_f, W), Dict};
%% encode_arg({'char', C}, Dict) ->
%%     {flb_module:encode(?tag_h, C), Dict};
encode_arg({string, BinString}, Dict0) when is_binary(BinString) ->
    {Offset, Dict} = beam_dict:string(BinString, Dict0),
    {flb_module:encode(?tag_u, Offset), Dict};
encode_arg({extfunc, M, F, A}, Dict0) ->
    {Index, Dict} = beam_dict:import(M, F, A, Dict0),
    {flb_module:encode(?tag_u, Index), Dict};
encode_arg({list, List}, Dict0) ->
    {L, Dict} = flb_module:encode_list(List, Dict0, []),
    {[flb_module:encode(?tag_z, 1), flb_module:encode(?tag_u, length(List)) | L], Dict};
encode_arg({commands, List0}, Dict) ->
    List1 =
        [begin
             [H | T] = tuple_to_list(Tuple),
             [{atom, H} | T]
         end
         || Tuple <- List0],
    List = lists:append(List1),
    encode_arg({list, List}, Dict);
encode_arg({float, Float}, Dict) when is_float(Float) ->
    flb_module:encode_literal(Float, Dict);
encode_arg({fr, Fr}, Dict) ->
    {[flb_module:encode(?tag_z, 2), flb_module:encode(?tag_u, Fr)], Dict};
encode_arg({field_flags, Flags0}, Dict) ->
    Flags = lists:foldl(fun(F, S) -> S bor flb_module:flag_to_bit(F) end, 0, Flags0),
    {flb_module:encode(?tag_u, Flags), Dict};
encode_arg({alloc, List}, Dict) ->
    flb_module:encode_alloc_list(List, Dict);
encode_arg({literal, Lit}, Dict) ->
    if Lit =:= [] ->
           encode_arg(nil, Dict);
       is_atom(Lit) ->
           encode_arg({atom, Lit}, Dict);
       is_integer(Lit) ->
           encode_arg({integer, Lit}, Dict);
       true ->
           flb_module:encode_literal(Lit, Dict)
    end;
encode_arg(Int, Dict) when is_integer(Int) ->
    {flb_module:encode(?tag_u, Int), Dict}.
