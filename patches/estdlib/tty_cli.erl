%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2005-2023. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%

%%
%% This file is almost copied verbatem from Erlang's lib/ssh/src/ssh_cli.erl.
%% Last compared with OTP-26.0.2 and https://github.com/erlang/otp/pull/7499

%%
%% Description: a gen_server implementing a simple
%% terminal (using the group module) for a CLI

-module(tty_cli).

-include("tty_pty.hrl").

-export([io_request/4,to_group/2]).


to_group([], _Group) ->
    ok;
to_group([$\^C | Tail], Group) ->
    exit(Group, interrupt),
    to_group(Tail, Group);
to_group(Data, Group) ->
    Func = fun(C) -> C /= $\^C end,
    Tail = case lists:splitwith(Func, Data) of
        {[], Right} ->
            Right;
        {Left, Right} ->
            Group ! {self(), {data, Left}},
            Right
    end,
    to_group(Tail, Group).

%%--------------------------------------------------------------------
%%% io_request, handle io requests from the user process,
%%% Note, this is not the real I/O-protocol, but the mockup version
%%% used between edlin and a user_driver. The protocol tags are
%%% similar, but the message set is different.
%%% The protocol only exists internally between edlin and a character
%%% displaying device...
%%% We are *not* really unicode aware yet, we just filter away characters
%%% beyond the latin1 range. We however handle the unicode binaries...
io_request({window_change, OldTty}, Buf, Tty, _Group) ->
    window_change(Tty, OldTty, Buf);
io_request({put_chars, Cs}, Buf, Tty, _Group) ->
    put_chars(bin_to_list(Cs), Buf, Tty);
io_request({put_chars, unicode, Cs}, Buf, Tty, _Group) ->
    put_chars(unicode:characters_to_list(Cs,unicode), Buf, Tty);
io_request({put_expand_no_trim, unicode, Expand}, Buf, Tty, _Group) ->
    insert_chars(unicode:characters_to_list(Expand, unicode), Buf, Tty);
io_request({insert_chars, Cs}, Buf, Tty, _Group) ->
    insert_chars(bin_to_list(Cs), Buf, Tty);
io_request({insert_chars, unicode, Cs}, Buf, Tty, _Group) ->
    insert_chars(unicode:characters_to_list(Cs,unicode), Buf, Tty);
io_request({move_rel, N}, Buf, Tty, _Group) ->
    move_rel(N, Buf, Tty);
io_request({move_line, N}, Buf, Tty, _Group) ->
    move_line(N, Buf, Tty);
io_request({move_combo, L, V, R}, Buf, Tty, _Group) ->
    {ML, Buf1} = move_rel(L, Buf, Tty),
    {MV, Buf2} = move_line(V, Buf1, Tty),
    {MR, Buf3} = move_rel(R, Buf2, Tty),
    {[ML,MV,MR], Buf3};
io_request(new_prompt, _Buf, _Tty, _Group) ->
    {[], {[], {[],[]}, [], 0 }};
io_request(delete_line, {_, {_, _}, _, Col}, Tty, _Group) ->
    MoveToBeg = move_cursor(Col, 0, Tty),
    {[MoveToBeg, "\e[J"],
     {[],{[],[]},[],0}};
io_request({redraw_prompt, Pbs, Pbs2, {LB, {Bef, Aft}, LA}}, Buf, Tty, _Group) ->
    {ClearLine, Cleared} = io_request(delete_line, Buf, Tty, _Group),
    CL = lists:reverse(Bef,Aft),
    Text = Pbs ++ lists:flatten(lists:join("\n"++Pbs2, lists:reverse(LB)++[CL|LA])),
    Moves = if LA /= [] ->
                    [Last|_] = lists:reverse(LA),
                    {move_combo, -length(Last), -length(LA), length(Bef)};
               true ->
                    {move_rel, -length(Aft)}
            end,
    {T, InsertedText} = io_request({insert_chars, unicode:characters_to_binary(Text)}, Cleared, Tty, _Group),
    {M, Moved} = io_request(Moves, InsertedText, Tty, _Group),
    {[ClearLine, T, M], Moved};
io_request({delete_chars,N}, Buf, Tty, _Group) ->
    delete_chars(N, Buf, Tty);
io_request(clear, Buf, _Tty, _Group) ->
    {"\e[H\e[2J", Buf};
io_request(beep, Buf, _Tty, _Group) ->
    {[7], Buf};

%% New in R12
io_request({get_geometry,columns},Buf,Tty, _Group) ->
    {ok, Tty#tty_pty.width, Buf};
io_request({get_geometry,rows},Buf,Tty, _Group) ->
    {ok, Tty#tty_pty.height, Buf};
io_request({requests,Rs}, Buf, Tty, Group) ->
    io_requests(Rs, Buf, Tty, [], Group);
io_request(tty_geometry, Buf, Tty, Group) ->
    io_requests([{move_rel, 0}, {put_chars, unicode, [10]}],
                Buf, Tty, [], Group);

%% New in 18
io_request({put_chars_sync, Class, Cs, Reply}, Buf, Tty, Group) ->
    %% We handle these asynchronous for now, if we need output guarantees
    %% we have to handle these synchronously
    Group ! {reply, Reply, ok},
    io_request({put_chars, Class, Cs}, Buf, Tty, Group);

io_request(_R, Buf, _Tty, _Group) ->
    {[], Buf}.

io_requests([R|Rs], Buf, Tty, Acc, Group) ->
    {Chars, NewBuf} = io_request(R, Buf, Tty, Group),
    io_requests(Rs, NewBuf, Tty, [Acc|Chars], Group);
io_requests([], Buf, _Tty, Acc, _Group) ->
    {Acc, Buf}.

%%% return commands for cursor navigation, assume everything is ansi
%%% (vt100), add clauses for other terminal types if needed
ansi_tty(N, L) ->
    ["\e[", integer_to_list(N), L].

get_tty_command(up, N, _TerminalType) ->
    ansi_tty(N, $A);
get_tty_command(down, N, _TerminalType) ->
    ansi_tty(N, $B);
get_tty_command(right, N, _TerminalType) ->
    ansi_tty(N, $C);
get_tty_command(left, N, _TerminalType) ->
    ansi_tty(N, $D).


-define(PAD, 10).
-define(TABWIDTH, 8).

%% convert input characters to buffer and to writeout
%% Note that the buf is reversed but the buftail is not
%% (this is handy; the head is always next to the cursor)
conv_buf([], {LB, {Bef, Aft}, LA, Col}, AccWrite, _Tty) ->
    {{LB, {Bef, Aft}, LA}, lists:reverse(AccWrite), Col};
conv_buf([13, 10 | Rest], {LB, {Bef, Aft}, LA, Col}, AccWrite, Tty = #tty_pty{width = W}) ->
    conv_buf(Rest, {[lists:reverse(Bef)|LB], {[], tl2(Aft)}, LA, Col+(W-(Col rem W))}, [10, 13 | AccWrite], Tty);
conv_buf([13 | Rest], {LB, {Bef, Aft}, LA, Col}, AccWrite, Tty = #tty_pty{width = W}) ->
    conv_buf(Rest, {[lists:reverse(Bef)|LB], {[], tl1(Aft)}, LA, Col+(W-(Col rem W))}, [13 | AccWrite], Tty);
conv_buf([10 | Rest],{LB, {Bef, Aft}, LA, Col}, AccWrite0, Tty = #tty_pty{width = W}) ->
    AccWrite =
        case pty_opt(onlcr,Tty) of
            0 -> [10 | AccWrite0];
            1 -> [10,13 | AccWrite0];
            undefined -> [10 | AccWrite0]
        end,
    conv_buf(Rest, {[lists:reverse(Bef)|LB], {[], tl1(Aft)}, LA, Col+(W - (Col rem W))}, AccWrite, Tty);
conv_buf([C | Rest], {LB, {Bef, Aft}, LA, Col}, AccWrite, Tty) ->
    conv_buf(Rest, {LB, {[C|Bef], tl1(Aft)}, LA, Col+1}, [C | AccWrite], Tty).

%%% put characters before the prompt
put_chars(Chars, Buf, Tty) ->
    case Buf of
        {[],{[],[]},[],_} -> {_, WriteBuf, _} = conv_buf(Chars, Buf, [], Tty),
            {WriteBuf, Buf};
        _ ->
            {Delete, DeletedState} = io_request(delete_line, Buf, Tty, []),
            {_, PutBuffer, _} = conv_buf(Chars, DeletedState, [], Tty),
            {Redraw, _} = io_request(redraw_prompt_pre_deleted, Buf, Tty, []),
            {[Delete, PutBuffer, Redraw], Buf}
    end.

%%% insert character at current position
insert_chars([], Buf, _Tty) ->
    {[], Buf};
insert_chars(Chars, {_LB,{_Bef, Aft},LA, _Col}=Buf, Tty) ->
    {{NewLB, {NewBef, _NewAft}, _NewLA}, WriteBuf, NewCol} = conv_buf(Chars, Buf, [], Tty),
    M = move_cursor(special_at_width(NewCol+length(Aft), Tty), NewCol, Tty),
    {[WriteBuf, Aft | M], {NewLB,{NewBef, Aft},LA, NewCol}}.

%%% delete characters at current position, (backwards if negative argument)
delete_chars(0, {LB,{Bef, Aft},LA, Col}, _Tty) ->
    {[], {LB,{Bef, Aft},LA, Col}};
delete_chars(N, {LB,{Bef, Aft},LA, Col}, Tty) when N > 0 ->
    NewAft = nthtail(N, Aft),
    M = move_cursor(Col + length(NewAft) + N, Col, Tty),
    {[NewAft, lists:duplicate(N, $ ) | M],
     {LB,{Bef, NewAft},LA, Col}};
delete_chars(N, {LB,{Bef, Aft},LA, Col}, Tty) -> % N < 0
    NewBef = nthtail(-N, Bef),
    NewCol = case Col + N of V when V >= 0 -> V; _ -> 0 end,
    M1 = move_cursor(Col, NewCol, Tty),
    M2 = move_cursor(special_at_width(NewCol+length(Aft)-N, Tty), NewCol, Tty),
    {[M1, Aft, lists:duplicate(-N, $ ) | M2],
     {LB,{NewBef, Aft},LA, NewCol}}.

%%% Window change, redraw the current line (and clear out after it
%%% if current window is wider than previous)
window_change(Tty, OldTty, Buf)
  when OldTty#tty_pty.width == Tty#tty_pty.width ->
     %% No line width change
    {[], Buf};
window_change(Tty, OldTty, {LB, {Bef, Aft}, LA, Col}) ->
    case OldTty#tty_pty.width - Tty#tty_pty.width of
        0 ->
            %% No line width change
            {[], {LB, {Bef, Aft}, LA, Col}};

        DeltaW0 when DeltaW0 < 0,
                     Aft == [] ->
            % Line width is decreased, cursor is at end of input
            {[], {LB, {Bef, Aft}, LA, Col}};

        DeltaW0 when DeltaW0 < 0,
                     Aft =/= [] ->
            % Line width is decreased, cursor is not at end of input
            {[], {LB, {Bef, Aft}, LA, Col}};

        DeltaW0 when DeltaW0 > 0 ->
            % Line width is increased
            {[], {LB, {Bef, Aft}, LA, Col}}
        end.

%% move around in buffer, respecting pad characters
step_over(0, {LB, {Bef, [?PAD |Aft]}, LA, Col}) ->
    {LB, {[?PAD | Bef], Aft}, LA, Col+1};
step_over(0, {LB, {Bef, Aft}, LA, Col}) ->
    {LB, {Bef, Aft}, LA, Col};
step_over(N, {LB, {[C | Bef], Aft}, LA, Col}) when N < 0 ->
    N1 = ifelse(C == ?PAD, N, N+1),
    step_over(N1, {LB, {Bef, [C | Aft]}, LA, Col-1});
step_over(N, {LB, {Bef, [C | Aft]}, LA, Col}) when N > 0 ->
    N1 = ifelse(C == ?PAD, N, N-1),
    step_over(N1, {LB, {[C | Bef], Aft}, LA, Col+1}).

%%% col and row from position with given width
col(N, W) -> N rem W.
row(N, W) -> N div W.

%%% move relative N characters
move_rel(N, {_LB, {_Bef, _Aft}, _LA, Col}=Buf, Tty) ->
    {NewLB, {NewBef, NewAft}, NewLA, NewCol} = step_over(N, Buf),
    M = move_cursor(Col, NewCol, Tty),
    {M, {NewLB, {NewBef, NewAft}, NewLA, NewCol}}.

move_line(V, {_LB, {_Bef, _Aft}, _LA, Col}, Tty = #tty_pty{width=W})
        when V < 0, length(_LB) >= -V ->
    {LinesJumped, [B|NewLB]} = lists:split(-V -1, _LB),
    CL = lists:reverse(_Bef,_Aft),
    NewLA = lists:reverse([CL|LinesJumped], _LA),
    {NewBB, NewAft} = lists:split(min(length(_Bef),length(B)), B),
    NewBef = lists:reverse(NewBB),
    NewCol = Col - length(_Bef) - lists:sum([((length(L)-1) div W)*W + W || L <- [B|LinesJumped]]) + length(NewBB),
    M = move_cursor(Col, NewCol, Tty),
    {M, {NewLB, {NewBef, NewAft}, NewLA, NewCol}};
move_line(V, {_LB, {_Bef, _Aft}, _LA, Col}, Tty = #tty_pty{width=W})
        when V > 0, length(_LA) >= V ->
    {LinesJumped, [A|NewLA]} = lists:split(V -1, _LA),
    CL = lists:reverse(_Bef,_Aft),
    NewLB = lists:reverse([CL|LinesJumped],_LB),
    {NewBB, NewAft} = lists:split(min(length(_Bef),length(A)), A),
    NewBef = lists:reverse(NewBB),
    NewCol = Col - length(_Bef) + lists:sum([((length(L)-1) div W)*W + W || L <- [CL|LinesJumped]]) + length(NewBB),
    M = move_cursor(Col, NewCol, Tty),
    {M, {NewLB, {NewBef, NewAft}, NewLA, NewCol}};
move_line(_, Buf, _) ->
    {"", Buf}.
%%% give move command for tty
move_cursor(A, A, _Tty) ->
    [];
move_cursor(From, To, #tty_pty{width=Width, term=Type}) ->
    Tcol = case col(To, Width) - col(From, Width) of
	       0 -> "";
	       I when I < 0 -> get_tty_command(left, -I, Type);
	       I -> get_tty_command(right, I, Type)
	end,
    Trow = case row(To, Width) - row(From, Width) of
	       0 -> "";
	       J when J < 0 -> get_tty_command(up, -J, Type);
	       J -> get_tty_command(down, J, Type)
	   end,
    [Tcol | Trow].

%%% Caution for line "breaks"
special_at_width(From0, #tty_pty{width=Width}) when (From0 rem Width) == 0 -> From0 - 1;
special_at_width(From0, _) -> From0.

%%% tail, works with empty lists
tl1([_|A]) -> A;
tl1(_) -> [].

%%% second tail
tl2([_,_|A]) -> A;
tl2(_) -> [].

%%% nthtail as in lists, but no badarg if n > the length of list
nthtail(0, A) -> A;
nthtail(N, [_ | A]) when N > 0 -> nthtail(N-1, A);
nthtail(_, _) -> [].

ifelse(Cond, A, B) ->
    case Cond of
	true -> A;
	_ -> B
    end.

bin_to_list(B) when is_binary(B) ->
    binary_to_list(B);
bin_to_list(L) when is_list(L) ->
    lists:flatten([bin_to_list(A) || A <- L]);
bin_to_list(I) when is_integer(I) ->
    I.

%%%----------------------------------------------------------------
pty_opt(Name, Tty) ->
    try
        proplists:get_value(Name, Tty#tty_pty.modes, undefined)
    catch
        _:_ -> undefined
    end.
