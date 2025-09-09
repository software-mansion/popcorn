-module(application_master).

-export([relay_to_group_leader/2]).

-compile({popcorn_patch_private, relay_to_group_leader/2}).

-record(state, {child, appl_data, children = [], procs = 0, gleader, req = []}).

% Patch reason: There's no user nor init processes (kernel app should start it),
% so instead of forwarding, handle the IO request here, preventing inifnite loop
% of relay to self
relay_to_group_leader(IoReq, State) ->
    Self = self(),
    % New clause
    if
        State#state.gleader =:= Self ->
            handle_io_request(IoReq);
        State#state.gleader =:= init ->
            case whereis(user) of
                undefined ->
                    State#state.gleader ! IoReq;
                User ->
                    User ! IoReq
            end;
        true ->
            State#state.gleader ! IoReq
    end.

% TODO: handle MFA put_chars request
% TODO: implement input requests
handle_io_request({io_request, Sender, ReplyAs, {put_chars, _Encoding, Chars}}) ->
    console:print(Chars),
    Sender ! {io_reply, ReplyAs, ok};
handle_io_request({io_request, _Sender, _ReplyAs, _Request}) ->
    erlang:error(not_implemented).
