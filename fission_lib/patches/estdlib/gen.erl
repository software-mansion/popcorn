%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2023. All Rights Reserved.
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
-module(gen).

-export([do_call/4]).

-compile({flb_patch_private, do_call/4}).

-dialyzer({no_improper_lists, do_call/4}).

% Patch reason: the last clause of do_call uses erlang:monitor/3
% which requires process aliases, not avaliable in AtomVM.
% Without it, if the calling process traps exits, there's RC and reply
% messages that arrived after the timeout may pollute its mailbox.
%
% Patch reason: the last clause uses erlang:send/3, not available
% in AtomVM. Since it only matters in distribution, which is not
% supported anyway, it's safe to change it to a regular send.

do_call(Process, _Label, _Request, _Timeout) when Process =:= self() ->
    exit(calling_self);
do_call(Process, Label, Request, infinity)
  when (is_pid(Process)
        andalso (node(Process) == node()))
       orelse (element(2, Process) == node()
               andalso is_atom(element(1, Process))
               andalso (tuple_size(Process) =:= 2)) ->
    Mref = erlang:monitor(process, Process),
    %% Local without timeout; no need to use alias since we unconditionally
    %% will wait for either a reply or a down message which corresponds to
    %% the process being terminated (as opposed to 'noconnection')...
    Process ! {Label, {self(), Mref}, Request},
    receive
        {Mref, Reply} ->
            erlang:demonitor(Mref, [flush]),
            {ok, Reply};
        {'DOWN', Mref, _, _, Reason} ->
            exit(Reason)
    end;
do_call(Process, Label, Request, Timeout) when is_atom(Process) =:= false ->
    % Mref = erlang:monitor(process, Process, [{alias,demonitor}]),
    Mref = erlang:monitor(process, Process),

    % Tag = [alias | Mref],

    %% OTP-24:
    %% Using alias to prevent responses after 'noconnection' and timeouts.
    %% We however still may call nodes responding via process identifier, so
    %% we still use 'noconnect' on send in order to try to send on the
    %% monitored connection, and not trigger a new auto-connect.
    %%
    % erlang:send(Process, {Label, {self(), Tag}, Request}, [noconnect]),
    erlang:send(Process, {Label, {self(), Mref}, Request}),

    receive
        % {[alias | Mref], Reply} ->
        {Mref, Reply} ->
            erlang:demonitor(Mref, [flush]),
            {ok, Reply};
        {'DOWN', Mref, _, _, noconnection} ->
            Node = flb_module:get_node(Process),
            exit({nodedown, Node});
        {'DOWN', Mref, _, _, Reason} ->
            exit(Reason)
    after Timeout ->
            erlang:demonitor(Mref, [flush]),
            receive
                % {[alias | Mref], Reply} ->
                {Mref, Reply} ->
                    {ok, Reply}
            after 0 ->
                    exit(timeout)
            end
    end.
