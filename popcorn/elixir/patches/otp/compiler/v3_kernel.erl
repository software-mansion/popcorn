-module(v3_kernel).

-compile({popcorn_patch_private, integer_fits_and_is_expandable/2}).
-compile({popcorn_patch_private, select_bin_int/1}).

-include("v3_kernel.hrl").
-include("core_parse.hrl").

% Patch reason: for some reason, the compiler converts binaries
% smaller than this to integers (and probably reconstructs them
% later somehow). Since AtomVM doesn't support big ints, it cuts
% off parts of binaries, so we need to limit it to fit in a regular
% integer, and patch functions that rely on it.

%% Matches expansion max segment in v3_core.
-define(EXPAND_MAX_SIZE_SEGMENT, 64).

-record(ivalues, {anno=[],args}).
-record(ifun, {anno=[],vars,body}).
-record(iset, {anno=[],vars,arg,body}).
-record(iletrec, {anno=[],defs}).
-record(ialias, {anno=[],vars,pat}).
-record(iclause, {anno=[],isub,osub,pats,guard,body}).

integer_fits_and_is_expandable(Int, Size) when is_integer(Int), is_integer(Size),
                                               0 < Size, Size =< ?EXPAND_MAX_SIZE_SEGMENT ->
    case <<Int:Size>> of
	<<Int:Size>> -> true;
	_ -> false
    end;
integer_fits_and_is_expandable(_Int, _Size) ->
    false.

select_bin_int([#iclause{pats=[#k_bin_seg{anno=A,type=integer,
                                          size=#k_literal{val=Bits0}=Sz,unit=U,
                                          flags=Fl,seg=#k_literal{val=Val},
                                          next=N}|Ps]}=C|Cs0]) when is_integer(Bits0) ->
    Bits = U * Bits0,
    if
	Bits > ?EXPAND_MAX_SIZE_SEGMENT -> throw(not_possible); %Expands the code too much.
	true -> ok
    end,
    popcorn_module:select_assert_match_possible(Bits, Val, Fl),
    P = #k_bin_int{anno=A,size=Sz,unit=U,flags=Fl,val=Val,next=N},
    case lists:member(native, Fl) of
	true -> throw(not_possible);
	false -> ok
    end,
    Cs1 = [C#iclause{pats=[P|Ps]}|popcorn_module:select_bin_int_1(Cs0, Bits, Fl, Val)],
    Cs = popcorn_module:reorder_bin_ints(Cs1),
    [{k_bin_int,Cs}];
select_bin_int(_) -> throw(not_possible).
