-module(eval_bits).

-export([expr_grp/3,expr_grp/4]).

-define(STACKTRACE,
        element(2, erlang:process_info(self(), current_stacktrace))).

%% Patch reason: in AVM the option "-unit:Size" inside bitstrings is not supported
expr_grp(Fields, Bindings, EvalFun, ErrorFun) ->
    expr_grp(Fields, Bindings, EvalFun, ErrorFun, <<>>).

expr_grp(Fields, Bindings, EvalFun) ->
    expr_grp(Fields, Bindings, EvalFun, fun flb_module:default_error/3, <<>>).

expr_grp(FS, Bs0, EvalFun, ErrorFun, Acc) ->
    {ListOfEvalField,Bs1} = expr_grp1(FS, Bs0, EvalFun, ErrorFun, [], 1),
    {value,flb_module:create_binary(ListOfEvalField, Acc),Bs1}.

expr_grp1([Field | FS], Bs0, EvalFun, ErrorFun, ListOfEvalField, Pos) ->
    {EvalField,Bs} = eval_field(Field, Bs0, EvalFun, ErrorFun, Pos),
    expr_grp1(FS, Bs, EvalFun, ErrorFun, [EvalField|ListOfEvalField], Pos + 1);
expr_grp1([], Bs, _EvalFun, _ErrorFun, ListOfFieldData, _Pos) ->
    {lists:reverse(ListOfFieldData),Bs}.

eval_field({bin_element, _, {string, _, S}, {integer,_,8}, [integer,{unit,1},unsigned,big]}, Bs0, _Fun, _ErrorFun, _Pos) ->
    Latin1 = [C band 16#FF || C <- S],
    {fun() -> list_to_binary(Latin1) end,Bs0};
eval_field({bin_element, _, {string, _, S}, default, default}, Bs0, _Fun, _ErrorFun, _Pos) ->
    Latin1 = [C band 16#FF || C <- S],
    {fun() ->list_to_binary(Latin1) end,Bs0};
eval_field({bin_element, Anno, {string, _, S}, Size0, Options0}, Bs0, Fun, ErrorFun, Pos) ->
    {Size1,[Type,{unit,Unit},Sign,Endian]} =
        flb_module:make_bit_type(Anno, Size0, Options0, ErrorFun),
    {value,Size,Bs1} = Fun(Size1, Bs0),
    {fun() ->
        Res = << <<(eval_exp_field1(Anno, C, Size, Unit,
            Type, Endian, Sign, ErrorFun, Pos))/bitstring>> ||
            C <- S >>,
        case S of
            "" ->
                _ = eval_exp_field1(Anno, 0, Size, Unit, Type, Endian, Sign, ErrorFun, Pos),
                ok;
            _ ->
                ok
        end,
        Res
     end,Bs1};
eval_field({bin_element,Anno,E,Size0,Options0}, Bs0, Fun, ErrorFun, Pos) ->
    {value,V,Bs1} = Fun(E, Bs0),
    {Size1,[Type,{unit,Unit},Sign,Endian]} =
        flb_module:make_bit_type(Anno, Size0, Options0, ErrorFun),
    {value,Size,Bs} = Fun(Size1, Bs1),
    {fun() -> eval_exp_field1(Anno, V, Size, Unit, Type, Endian, Sign, ErrorFun, Pos) end,Bs}.

eval_exp_field1(Anno, V, Size, Unit, Type, Endian, Sign, ErrorFun, Pos) ->
    try
        eval_exp_field(V, Size, Unit, Type, Endian, Sign)
    catch
        error:system_limit:Stacktrace ->
            ErrorFun(Anno, system_limit, flb_module:add_eval_pos_to_error_info(Stacktrace, Pos));
        error:_:Stacktrace ->
            ErrorFun(Anno, badarg, flb_module:add_eval_pos_to_error_info(Stacktrace, Pos))
    end.

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
      Size1 = Size div 8,
	    <<Val:Size1/binary>>;
	_ ->
	    erlang:raise(error, badarg, ?STACKTRACE)
    end;
eval_exp_field(Val, Size, Unit, binary, _, _) ->
    case Size rem 8 of
        0 ->
            Size1 = Size div 8,
            <<Val:(Size1*Unit)/binary>>;
        _ ->
%%            This is still unsupported in AtomVM
            <<Val:(Size*Unit)/binary-unit:1>>
    end.
