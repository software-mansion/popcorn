%% Patch reason: bitstring modifiers with size for binary matching and creation
%% are not supported in AVM
%% Particular changes are marked with a `Patch reason: bitstrings` comment
%% These patches can be removed when https://github.com/atomvm/AtomVM/pull/1978
%% is merged and downstreamed

-module(eval_bits).

-compile({popcorn_patch_private, eval_exp_field/6}).
-compile({popcorn_patch_private, get_value/6}).

-define(STACKTRACE,
        element(2, erlang:process_info(self(), current_stacktrace))).

eval_exp_field(Val, Size, Unit, integer, little, signed) ->
    <<Val:(Size*Unit)/little-signed>>;
eval_exp_field(Val, Size, Unit, integer, little, unsigned) ->
    <<Val:(Size*Unit)/little>>;
eval_exp_field(Val, Size, Unit, integer, native, signed) ->
    <<Val:(Size*Unit)/native-signed>>;
eval_exp_field(Val, Size, Unit, integer, native, unsigned) ->
    <<Val:(Size*Unit)/native>>;
eval_exp_field(Val, Size, Unit, integer, big, signed) ->
    <<Val:(Size*Unit)/signed>>;
eval_exp_field(Val, Size, Unit, integer, big, unsigned) ->
    <<Val:(Size*Unit)>>;
eval_exp_field(Val, _Size, _Unit, utf8, _, _) ->
    <<Val/utf8>>;
eval_exp_field(Val, _Size, _Unit, utf16, big, _) ->
    <<Val/big-utf16>>;
eval_exp_field(Val, _Size, _Unit, utf16, little, _) ->
    <<Val/little-utf16>>;
eval_exp_field(Val, _Size, _Unit, utf16, native, _) ->
    <<Val/native-utf16>>;
eval_exp_field(Val, _Size, _Unit, utf32, big, _) ->
    <<Val/big-utf32>>;
eval_exp_field(Val, _Size, _Unit, utf32, little, _) ->
    <<Val/little-utf32>>;
eval_exp_field(Val, _Size, _Unit, utf32, native, _) ->
    <<Val/native-utf32>>;
eval_exp_field(Val, Size, Unit, float, little, _) ->
    <<Val:(Size*Unit)/float-little>>;
eval_exp_field(Val, Size, Unit, float, native, _) ->
    <<Val:(Size*Unit)/float-native>>;
eval_exp_field(Val, Size, Unit, float, big, _) ->
    <<Val:(Size*Unit)/float>>;
eval_exp_field(Val, all, Unit, binary, _, _) ->
    case erlang:bit_size(Val) of
	Size when Size rem Unit =:= 0 ->
      % Patch reason: bitstrings
      Size1 = Size div 8,
	  <<Val:Size1/binary>>;
	_ ->
	    erlang:raise(error, badarg, ?STACKTRACE)
    end;
eval_exp_field(Val, Size, Unit, binary, _, _) ->
    % Patch reason: bitstrings
    case (Size * Unit) rem 8 of
        0 ->
            Size1 = (Size * Unit) div 8,
            <<Val:Size1/binary>>;
        _ ->
            % This is still unsupported in AtomVM
            <<Val:(Size*Unit)/binary-unit:1>>
    end.


get_value(Bin, integer, Size, Unit, Sign, Endian) ->
    popcorn_module:get_integer(Bin, Size*Unit, Sign, Endian);
get_value(Bin, float, Size, Unit, _Sign, Endian) ->
    popcorn_module:get_float(Bin, Size*Unit, Endian);
get_value(Bin, utf8, undefined, _Unit, _Sign, _Endian) ->
    <<I/utf8,Rest/bits>> = Bin,
    {I,Rest};
get_value(Bin, utf16, undefined, _Unit, _Sign, big) ->
    <<I/big-utf16,Rest/bits>> = Bin,
    {I,Rest};
get_value(Bin, utf16, undefined, _Unit, _Sign, little) ->
    <<I/little-utf16,Rest/bits>> = Bin,
    {I,Rest};
get_value(Bin, utf16, undefined, _Unit, _Sign, native) ->
    <<I/native-utf16,Rest/bits>> = Bin,
    {I,Rest};
get_value(Bin, utf32, undefined, _Unit, _Sign, big) ->
    <<Val/big-utf32,Rest/bits>> = Bin,
    {Val,Rest};
get_value(Bin, utf32, undefined, _Unit, _Sign, little) ->
    <<Val/little-utf32,Rest/bits>> = Bin,
    {Val,Rest};
get_value(Bin, utf32, undefined, _Unit, _Sign, native) ->
    <<Val/native-utf32,Rest/bits>> = Bin,
    {Val,Rest};
get_value(Bin, binary, all, Unit, _Sign, _Endian) ->
    0 = (bit_size(Bin) rem Unit),
    {Bin,<<>>};
get_value(Bin, binary, Size, Unit, _Sign, _Endian) ->
    % Patch reason: bitstrings
    case (Size * Unit) rem 8 of
        0 ->
            TotSize = (Size * Unit) div 8,
            <<Val:TotSize/binary,Rest/bits>> = Bin,
            {Val, Rest};
        _ ->
            % This is still unsupported in AtomVM
            TotSize = Size * Unit,
            <<Val:TotSize/bitstring,Rest/bits>> = Bin,
            {Val, Rest}
    end.
