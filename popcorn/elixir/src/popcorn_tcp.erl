%% TCP connections over Popcorn's JavaScript bridge. gen_tcp uses this module
%% for the connections it opens with the `{tcp_module, popcorn_tcp}` option,
%% and JavaScript carries them: see Popcorn.init's `tcp` option. The VM has no
%% sockets of its own.
%%
%%     gen_tcp:connect("pglite", 5432, [binary, {tcp_module, popcorn_tcp}])
%%
%% Sends events using `wasm:send/1` and expects to receive `{wasm, Event}`:
%%
%%   #{type => connect, connection, host, port}   the VM opens a connection
%%   #{type => data, connection, data}            data, in either direction
%%   #{type => close, connection}                 the VM closes the connection
%%   #{type => connected, connection}             JavaScript opened it
%%   #{type => closed, connection, reason}        JavaScript closed it, or
%%                                                couldn't open it
%%
%% Sockets are `{'$inet', popcorn_tcp, Pid}`, which gen_tcp and inet pass to
%% this module's functions. Pid holds the connection, as a port does for
%% gen_tcp's own sockets: it buffers the data and delivers it to the
%% controlling process, in passive or active mode, binary or list. Only
%% `{packet, raw}` is supported.

-module(popcorn_tcp).

%% gen_tcp's tcp_module callbacks
-export([getaddrs/2, getserv/1, connect/4]).
%% Socket functions, which gen_tcp and inet dispatch to
-export([send/2, recv/2, recv/3, close/1, shutdown/2, controlling_process/2,
         setopts/2, getopts/2, peername/1, sockname/1]).

%% Internal exports
-export([socket_init/6]).

-define(ROUTER, popcorn_tcp).
-define(SOCKET(Pid), {'$inet', ?MODULE, Pid}).
%% What getopts reports for the buffer sizes, which JavaScript doesn't have
-define(BUFFER_SIZE, 65536).

%% ------------------------------------------------------------------
%% gen_tcp's tcp_module callbacks
%% ------------------------------------------------------------------

%% JavaScript resolves the host
getaddrs(Address, _Timer) ->
    {ok, [Address]}.

getserv(Port) when is_integer(Port) -> {ok, Port};
getserv(_Port) -> {error, einval}.

connect(Address, Port, Opts, Timeout) ->
    case parse_opts(Opts, #{active => true, mode => list}) of
        {ok, SocketOpts} ->
            router(),
            Pid = spawn(?MODULE, socket_init,
                        [self(), host(Address), Port, SocketOpts, Timeout, ?ROUTER]),
            Ref = monitor(process, Pid),
            receive
                {Pid, connected} ->
                    demonitor(Ref, [flush]),
                    {ok, ?SOCKET(Pid)};
                {Pid, {error, Reason}} ->
                    demonitor(Ref, [flush]),
                    {error, Reason};
                {'DOWN', Ref, process, Pid, _Reason} ->
                    {error, econnrefused}
            end;
        {error, _} = Error ->
            Error
    end.

host(Address) when is_list(Address) -> unicode:characters_to_binary(Address);
host(Address) when is_atom(Address) -> atom_to_binary(Address);
host(Address) when is_tuple(Address) -> list_to_binary(inet:ntoa(Address)).

%% ------------------------------------------------------------------
%% Socket functions
%% ------------------------------------------------------------------

send(?SOCKET(Pid), Data) ->
    call(Pid, {send, Data}).

recv(Socket, Length) ->
    recv(Socket, Length, infinity).

recv(?SOCKET(Pid), Length, Timeout) ->
    call(Pid, {recv, Length, Timeout}).

close(?SOCKET(Pid)) ->
    case call(Pid, close) of
        {error, closed} -> ok;
        Result -> Result
    end.

shutdown(?SOCKET(Pid), How) ->
    call(Pid, {shutdown, How}).

controlling_process(?SOCKET(Pid), NewOwner) ->
    call(Pid, {controlling_process, NewOwner}).

setopts(?SOCKET(Pid), Opts) ->
    call(Pid, {setopts, Opts}).

getopts(?SOCKET(Pid), Opts) ->
    call(Pid, {getopts, Opts}).

peername(?SOCKET(Pid)) ->
    call(Pid, peername).

sockname(?SOCKET(_Pid)) ->
    {ok, {{127, 0, 0, 1}, 0}}.

%% ------------------------------------------------------------------
%% Router: the endpoint JavaScript sends to, which dispatches each
%% connection's events to its socket process
%% ------------------------------------------------------------------

router() ->
    case whereis(?ROUTER) of
        undefined ->
            Caller = self(),
            Pid = spawn(fun() ->
                            Registered = try register(?ROUTER, self())
                                         catch error:badarg -> false
                                         end,
                            Caller ! {?ROUTER, self(), Registered},
                            Registered andalso router_loop(#{})
                        end),
            receive
                {?ROUTER, Pid, true} -> Pid;
                %% Another connection started it meanwhile
                {?ROUTER, Pid, false} -> router()
            end;
        Pid ->
            Pid
    end.

router_loop(Sockets) ->
    receive
        {route, Connection, Pid} ->
            router_loop(Sockets#{Connection => {Pid, monitor(process, Pid)}});

        {wasm, Event = #{<<"connection">> := Connection}} ->
            case Sockets of
                #{Connection := {Pid, _Ref}} -> Pid ! {?ROUTER, Event};
                #{} -> ok
            end,
            router_loop(Sockets);

        %% However the socket process stopped, JavaScript closes the connection.
        %% It ignores connections it already closed.
        {'DOWN', Ref, process, _Pid, _Reason} ->
            case [C || C := {_, R} <- Sockets, R =:= Ref] of
                [Connection] ->
                    to_js(#{type => close, connection => Connection}),
                    router_loop(maps:remove(Connection, Sockets));
                [] ->
                    router_loop(Sockets)
            end;

        _Other ->
            router_loop(Sockets)
    end.

%% ------------------------------------------------------------------
%% Socket process
%% ------------------------------------------------------------------

socket_init(Owner, Host, Port, Opts, Timeout, Router) ->
    Connection = binary:encode_hex(rand:bytes(16), lowercase),
    Router ! {route, Connection, self()},
    OwnerRef = monitor(process, Owner),
    to_js(#{type => connect, connection => Connection, host => Host, port => Port}),
    Timer = start_timer(Timeout, connect_timeout),
    receive
        {?ROUTER, #{<<"type">> := <<"connected">>}} ->
            cancel_timer(Timer),
            Owner ! {self(), connected},
            loop(Opts#{connection => Connection, port => Port, owner => Owner,
                       owner_ref => OwnerRef, buffer => <<>>, recv => undefined,
                       closed => false});
        {?ROUTER, Event = #{<<"type">> := <<"closed">>}} ->
            Owner ! {self(), {error, reason(Event, econnrefused)}};
        connect_timeout ->
            Owner ! {self(), {error, timeout}};
        {'DOWN', OwnerRef, process, Owner, _Reason} ->
            ok
    end.

loop(State) ->
    receive
        {Ref, From, Request} when is_reference(Ref), is_pid(From) ->
            handle_call(Request, Ref, From, State);

        {?ROUTER, #{<<"type">> := <<"data">>, <<"data">> := Data}} when is_binary(Data) ->
            #{buffer := Buffer} = State,
            deliver(State#{buffer := <<Buffer/binary, Data/binary>>});

        {?ROUTER, #{<<"type">> := <<"closed">>}} ->
            deliver(State#{closed := true});

        {recv_timeout, Ref} ->
            case State of
                #{recv := {Ref, From, _Length, _Timer}} ->
                    From ! {Ref, {error, timeout}},
                    loop(State#{recv := undefined});
                #{} ->
                    loop(State)
            end;

        %% A socket closes with the process that controls it
        {'DOWN', OwnerRef, process, _Owner, _Reason} when OwnerRef =:= map_get(owner_ref, State) ->
            ok;

        _Other ->
            loop(State)
    end.

handle_call({send, _Data}, Ref, From, State = #{closed := true}) ->
    reply(Ref, From, {error, closed}, State);
handle_call({send, Data}, Ref, From, State = #{connection := Connection}) ->
    try iolist_to_binary(Data) of
        Binary ->
            to_js(#{type => data, connection => Connection, data => wasm:bytes(Binary)}),
            reply(Ref, From, ok, State)
    catch error:badarg ->
        reply(Ref, From, {error, einval}, State)
    end;

handle_call({recv, _Length, _Timeout}, Ref, From, State = #{active := Active})
  when Active =/= false ->
    reply(Ref, From, {error, einval}, State);
handle_call({recv, _Length, _Timeout}, Ref, From, State = #{recv := {_, _, _, _}}) ->
    reply(Ref, From, {error, ealready}, State);
handle_call({recv, Length, Timeout}, Ref, From, State) ->
    Timer = start_timer(Timeout, {recv_timeout, Ref}),
    deliver(State#{recv := {Ref, From, Length, Timer}});

handle_call(close, Ref, From, _State) ->
    %% The router tells JavaScript, when this process is down
    From ! {Ref, ok};

handle_call({shutdown, _How}, Ref, From, State = #{connection := Connection}) ->
    to_js(#{type => close, connection => Connection}),
    reply(Ref, From, ok, State#{closed := true});

handle_call({controlling_process, NewOwner}, Ref, From, State = #{owner := Owner})
  when From =:= Owner ->
    #{owner_ref := OwnerRef} = State,
    demonitor(OwnerRef, [flush]),
    reply(Ref, From, ok, State#{owner := NewOwner, owner_ref := monitor(process, NewOwner)});
handle_call({controlling_process, _NewOwner}, Ref, From, State) ->
    reply(Ref, From, {error, not_owner}, State);

handle_call({setopts, Opts}, Ref, From, State) ->
    case parse_opts(Opts, maps:with([active, mode], State)) of
        {ok, Parsed} ->
            From ! {Ref, ok},
            deliver(maps:merge(State, Parsed));
        {error, _} = Error ->
            reply(Ref, From, Error, State)
    end;

handle_call({getopts, Opts}, Ref, From, State) ->
    Values = lists:filtermap(fun(Opt) -> getopt(Opt, State) end, Opts),
    reply(Ref, From, {ok, Values}, State);

handle_call(peername, Ref, From, State = #{port := Port}) ->
    reply(Ref, From, {ok, {{127, 0, 0, 1}, Port}}, State);

handle_call(_Request, Ref, From, State) ->
    reply(Ref, From, {error, einval}, State).

reply(Ref, From, Reply, State) ->
    From ! {Ref, Reply},
    loop(State).

%% Hands the buffered data to a waiting recv, or in active mode to the
%% controlling process, and tells either that the connection closed
deliver(State = #{recv := {Ref, From, Length, Timer}, buffer := Buffer, closed := Closed}) ->
    case Buffer of
        <<Data:Length/binary, Rest/binary>> when Length > 0 ->
            cancel_timer(Timer),
            From ! {Ref, {ok, data(Data, State)}},
            loop(State#{recv := undefined, buffer := Rest});
        <<_, _/binary>> when Length =:= 0 ->
            cancel_timer(Timer),
            From ! {Ref, {ok, data(Buffer, State)}},
            loop(State#{recv := undefined, buffer := <<>>});
        _ when Closed ->
            cancel_timer(Timer),
            From ! {Ref, {error, closed}},
            loop(State#{recv := undefined});
        _ ->
            loop(State)
    end;
deliver(State = #{active := false}) ->
    loop(State);
deliver(State = #{buffer := <<_, _/binary>> = Buffer, owner := Owner}) ->
    Owner ! {tcp, ?SOCKET(self()), data(Buffer, State)},
    deliver(next_active(State#{buffer := <<>>}));
deliver(#{closed := true, owner := Owner}) ->
    %% As gen_tcp's, the socket closes once the owner knows
    Owner ! {tcp_closed, ?SOCKET(self())};
deliver(State) ->
    loop(State).

next_active(State = #{active := once}) ->
    State#{active := false};
next_active(State = #{active := 1, owner := Owner}) ->
    Owner ! {tcp_passive, ?SOCKET(self())},
    State#{active := false};
next_active(State = #{active := N}) when is_integer(N) ->
    State#{active := N - 1};
next_active(State) ->
    State.

data(Binary, #{mode := binary}) -> Binary;
data(Binary, #{mode := list}) -> binary_to_list(Binary).

%% ------------------------------------------------------------------
%% Options
%% ------------------------------------------------------------------

%% The options that matter to this module, ignoring the others, like buffer
%% sizes or send_timeout, as JavaScript decides those
parse_opts(Opts, Acc) when is_list(Opts) ->
    try
        {ok, lists:foldl(fun parse_opt/2, Acc, Opts)}
    catch throw:einval ->
        {error, einval}
    end;
parse_opts(_Opts, _Acc) ->
    {error, einval}.

parse_opt(binary, Acc) -> Acc#{mode => binary};
parse_opt(list, Acc) -> Acc#{mode => list};
parse_opt({mode, Mode}, Acc) when Mode =:= binary; Mode =:= list -> Acc#{mode => Mode};
parse_opt({active, Active}, Acc)
  when is_boolean(Active); Active =:= once ->
    Acc#{active => Active};
parse_opt({active, N}, Acc = #{active := Current}) when is_integer(N) ->
    %% As gen_tcp's: N adds to a count, and the count turns passive at 0
    Count = case Current of
                C when is_integer(C) -> C + N;
                _ -> N
            end,
    if
        Count =< 0 -> Acc#{active => false};
        true -> Acc#{active => Count}
    end;
parse_opt({active, N}, Acc) when is_integer(N) ->
    parse_opt({active, N}, Acc#{active => false});
parse_opt({packet, Packet}, Acc) when Packet =:= raw; Packet =:= 0 -> Acc;
parse_opt({packet, _}, _Acc) -> throw(einval);
parse_opt(_Opt, Acc) -> Acc.

getopt(active, #{active := Active}) -> {true, {active, Active}};
getopt(mode, #{mode := Mode}) -> {true, {mode, Mode}};
getopt(packet, _State) -> {true, {packet, raw}};
getopt(Size, _State) when Size =:= buffer; Size =:= sndbuf; Size =:= recbuf ->
    {true, {Size, ?BUFFER_SIZE}};
getopt(_Opt, _State) -> false.

%% ------------------------------------------------------------------
%% Helpers
%% ------------------------------------------------------------------

call(Pid, Request) ->
    Ref = monitor(process, Pid),
    Pid ! {Ref, self(), Request},
    receive
        {Ref, Result} ->
            demonitor(Ref, [flush]),
            Result;
        {'DOWN', Ref, process, Pid, _Reason} ->
            {error, closed}
    end.

%% Only reasons the VM knows, like econnrefused, as JavaScript sends any string
reason(#{<<"reason">> := Reason}, Default) when is_binary(Reason) ->
    try binary_to_existing_atom(Reason)
    catch error:badarg -> Default
    end;
reason(_Event, Default) ->
    Default.

start_timer(infinity, _Message) -> undefined;
start_timer(Timeout, Message) -> erlang:send_after(Timeout, self(), Message).

cancel_timer(undefined) -> ok;
cancel_timer(Timer) -> erlang:cancel_timer(Timer).

to_js(Event) ->
    wasm:send(#{'_popcorn' => #{t => tcp, payload => Event}}).
