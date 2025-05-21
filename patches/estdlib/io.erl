%
% This file is part of AtomVM.
%
% Copyright 2018-2021 Davide Bettio <davide@uninstall.it>
% Copyright 2019 Fred Dushin <fred@dushin.net>
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
%
%    http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.
%
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
%

%%-----------------------------------------------------------------------------
%% @doc An implementation of the Erlang/OTP io interface.
%%
%% This module implements a strict subset of the Erlang/OTP io interface.
%% @end
%%-----------------------------------------------------------------------------
-module(io).
-compile({popcorn_patch_private, io_request/2}).
-export([format/1, format/2, fwrite/1, fwrite/2, get_line/1, put_chars/1, put_chars/2]).

%%-----------------------------------------------------------------------------
%% @doc     Equivalent to format(Format, []).
%% @end
%%-----------------------------------------------------------------------------
-spec format(Format :: string()) -> string().
format(Format) when is_list(Format) ->
    format(Format, []).

%%-----------------------------------------------------------------------------
%% @param   Format format string
%% @param   Args format argument
%% @returns string
%% @doc     Format string and data to console.
%%          See io_lib:format/2 for information about
%%          formatting capabilities.
%% @end
%%-----------------------------------------------------------------------------
-spec format(Format :: string(), Args :: list()) -> string().
format(Format, Args) when is_list(Format) andalso is_list(Args) ->
    Msg =
        try
            io_lib:format(Format, Args)
        catch
            _:_ ->
                io_lib:format("Bad format!  Format: ~p Args: ~p~n", [Format, Args])
        end,
    put_chars(Msg).

fwrite(Format) ->
    format(Format).

fwrite(Format, Args) ->
    format(Format, Args).

%%-----------------------------------------------------------------------------
%% @param   Prompt prompt for user input
%% @returns string
%% @doc     Read string from console with prompt.
%% @end
%%-----------------------------------------------------------------------------
-spec get_line(Prompt :: string()) -> string().
get_line(Prompt) ->
    Self = self(),
    case erlang:group_leader() of
        Self ->
            erlang:throw(no_group_leader);
        Leader ->
            Ref = make_ref(),
            Leader ! {io_request, self(), Ref, {get_line, unicode, Prompt}},
            receive
                {io_reply, Ref, Line} -> Line
            end
    end.

%%-----------------------------------------------------------------------------
%% @param   Chars character(s) to write to console
%% @returns ok
%% @doc     Writes the given character(s) to the console.
%% @end
%%-----------------------------------------------------------------------------
-spec put_chars(Chars :: list() | binary()) -> ok.
put_chars(Chars) ->
    Self = self(),
    case erlang:group_leader() of
        Self ->
            console:print(Chars);
        Leader ->
            Ref = make_ref(),
            Leader ! {io_request, self(), Ref, {put_chars, unicode, Chars}},
            receive
                {io_reply, Ref, Line} -> Line
            end
    end.

%%-----------------------------------------------------------------------------
%% @param   Chars character(s) to write to console
%% @returns ok
%% @doc     Writes the given character(s) to the console.
%% @end
%%-----------------------------------------------------------------------------
-spec put_chars(Device :: any(), Chars :: list() | binary()) -> ok.
put_chars(_Device, Chars) ->
    put_chars(Chars).

bc_req(Pid, Req0, MaybeConvert) ->
%%    for some reason unknown for me in the next line net_kernel:dflag_unicode_io/1 is failing in the VM and is not 
%%    properly covered by a patch 
%%    case net_kernel:dflag_unicode_io(Pid) of
    case true of
        true ->
            %% The most common case. A modern i/o server.
            {false,Req0};
        false ->
            %% Backward compatibility only. Unlikely to ever happen.
            case tuple_to_list(Req0) of
                [Op,_Enc] ->
                    {MaybeConvert,Op};
                [Op,_Enc|T] ->
                    Req = list_to_tuple([Op|T]),
                    {MaybeConvert,Req}
            end
    end.

io_request(Pid, {write,Term}) ->
    bc_req(Pid,{put_chars,unicode,io_lib,write,[Term]},false);
io_request(Pid, {format,Format,Args}) ->
    bc_req(Pid,{put_chars,unicode,io_lib,format,[Format,Args]},false);
io_request(Pid, {fwrite,Format,Args}) ->
    bc_req(Pid,{put_chars,unicode,io_lib,fwrite,[Format,Args]},false);
io_request(Pid, nl) ->
    bc_req(Pid,{put_chars,unicode,io_lib:nl()},false);
io_request(Pid, {put_chars,Enc,Chars}=Request0)
    when is_list(Chars), node(Pid) =:= node() ->
    %% Convert to binary data if the I/O server is guaranteed to be new
    Request =
        case catch unicode:characters_to_binary(Chars,Enc) of
            Binary when is_binary(Binary) ->
                {put_chars,Enc,Binary};
            _ ->
                Request0
        end,
    {false,Request};
io_request(Pid, {put_chars,Enc,Chars}=Request0)
    when is_list(Chars) ->
    case net_kernel:dflag_unicode_io(Pid) of
        true ->
            case catch unicode:characters_to_binary(Chars,Enc,unicode) of
                Binary when is_binary(Binary) ->
                    {false,{put_chars,unicode,Binary}};
                _ ->
                    {false,Request0}
            end;
        false ->
            %% Convert back to old style put_chars message...
            case catch unicode:characters_to_binary(Chars,Enc,latin1) of
                Binary when is_binary(Binary) ->
                    {false,{put_chars,Binary}};
                _ ->
                    {false,{put_chars,Chars}}
            end
    end;
io_request(Pid, {fread,Prompt,Format}) ->
    bc_req(Pid,{get_until,unicode,Prompt,io_lib,fread,[Format]},true);
io_request(Pid, {get_until,Enc,Prompt,M,F,A}) ->
    bc_req(Pid,{get_until,Enc,Prompt,M,F,A},true);
io_request(Pid, {get_chars,Enc,Prompt,N}) ->
    bc_req(Pid,{get_chars,Enc,Prompt,N},true);
io_request(Pid, {get_line,Enc,Prompt}) ->
    bc_req(Pid,{get_line,Enc,Prompt},true);
io_request(Pid, {get_password,Enc}) ->
    bc_req(Pid,{get_password, Enc},true);
io_request(_Pid, R) ->				%Pass this straight through
    {false,R}.
