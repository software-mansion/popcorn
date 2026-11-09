%% Erlang distribution over Popcorn's JavaScript bridge. Vibecoded.
%% Sends distribution events using `wasm:send/1` and expects to receive
%% `{wasm, Event}`. The events are:
%%
%%   #{type => connect, connection, from, to}    node From opens a connection to To
%%   #{type => message, connection, data}        data on the connection
%%   #{type => disconnect, connection}           the connection is closed
%%

-module(popcorn_dist).

-export([listen/2, accept/1, accept_connection/5, setup/5, close/1, select/1,
         is_node_name/1, address/0]).
-export([setopts/2, getopts/2]).

%% Internal exports
-export([router_init/0, accept_loop/1, do_accept/6, do_setup/6, controller_init/2,
         reader_init/0]).

-include_lib("kernel/include/net_address.hrl").
-include_lib("kernel/include/dist.hrl").
-include_lib("kernel/include/dist_util.hrl").

-define(ROUTER, popcorn_dist).
-define(FAMILY, popcorn).
-define(PROTOCOL, popcorn).

%% As in gen_tcp_dist: no lingering signal binaries
-define(SPAWN_OPTS, [{message_queue_data, off_heap}, {fullsweep_after, 0}]).

%% ------------------------------------------------------------------
%% Distribution module API
%% ------------------------------------------------------------------

%% Every node name with a host is ours: JavaScript resolves them
select(Node) -> is_node_name(Node).

is_node_name(Node) when is_atom(Node) ->
    case string:split(atom_to_list(Node), "@") of
        [Name, Host] when Name =/= [], Host =/= [] -> true;
        _ -> false
    end;
is_node_name(_Node) ->
    false.

address() ->
    #net_address{host = "popcorn", protocol = ?PROTOCOL, family = ?FAMILY}.

listen(Name, Host) ->
    Router = start_router(),
    Node = list_to_atom(atom_to_list(Name) ++ "@" ++ Host),
    {ok, {Router, (address())#net_address{address = Node, host = Host}, creation()}}.

%% net_kernel stops: the node no longer accepts connections, until a new
%% acceptor registers
close(Router) ->
    Router ! {acceptor, undefined},
    announce(null).

%% Incoming connections, which the router announces. The node is announced
%% only now, once it can accept them, and again when net_kernel restarts the
%% acceptor.
accept(Router) ->
    Acceptor = spawn_opt(?MODULE, accept_loop, [self()], [link, {priority, max}]),
    Router ! {acceptor, Acceptor},
    announce(node()),
    Acceptor.

accept_loop(Kernel) ->
    receive
        {incoming, Ctrl} ->
            Kernel ! {accept, self(), Ctrl, ?FAMILY, ?PROTOCOL},
            receive
                {Kernel, controller, Pid} ->
                    %% The transport may have closed the connection meanwhile,
                    %% taking the controller down: then the handshake fails at
                    %% once, rather than waiting for this
                    try call_ctrl(Ctrl, {supervisor, Pid})
                    catch exit:{?MODULE, _Reason} -> ok
                    end,
                    Pid ! {self(), controller};
                {Kernel, unsupported_protocol} ->
                    exit(unsupported_protocol)
            end,
            accept_loop(Kernel)
    end.

accept_connection(AcceptPid, Ctrl, MyNode, Allowed, SetupTime) ->
    spawn_opt(?MODULE, do_accept, [self(), AcceptPid, Ctrl, MyNode, Allowed, SetupTime],
              dist_util:net_ticker_spawn_options()).

do_accept(Kernel, AcceptPid, Ctrl, MyNode, Allowed, SetupTime) ->
    receive
        {AcceptPid, controller} ->
            Timer = dist_util:start_timer(SetupTime),
            HSData = (hs_data(Ctrl))#hs_data{kernel_pid = Kernel,
                                             this_node = MyNode,
                                             socket = Ctrl,
                                             timer = Timer,
                                             this_flags = 0,
                                             allowed = Allowed},
            dist_util:handshake_other_started(HSData)
    end.

%% Outgoing connections: JavaScript opens the connection, or closes it if it
%% can't reach the node. The router may not run yet, if the node doesn't
%% listen (`-dist_listen false`), and net_kernel calls this, as it does
%% listen/2, so starting it here doesn't race. Such a node is announced only
%% now, before its first connection, so that JavaScript can close its
%% connections when the VM exits. Announcing it again changes nothing. A
%% dynamic name, undefined@Host, isn't the node's: the first handshake gives
%% the node its name, which the controller announces then.
setup(Node, Type, MyNode, LongOrShortNames, SetupTime) ->
    _ = start_router(),
    case atom_to_list(MyNode) of
        "undefined@" ++ _ -> ok;
        _ -> announce(MyNode)
    end,
    spawn_opt(?MODULE, do_setup, [self(), Node, Type, MyNode, LongOrShortNames, SetupTime],
              dist_util:net_ticker_spawn_options()).

do_setup(Kernel, Node, Type, MyNode, _LongOrShortNames, SetupTime) ->
    Timer = dist_util:start_timer(SetupTime),
    Connection = new_connection(),
    {Ctrl, Reader} = spawn_connection(Connection),
    call_ctrl(Ctrl, {supervisor, self()}),
    ok = call_router({route, Connection, Ctrl, Reader}),
    to_js(#{type => connect, connection => Connection,
            from => atom_to_binary(MyNode), to => atom_to_binary(Node)}),
    HSData = (hs_data(Ctrl))#hs_data{kernel_pid = Kernel,
                                     other_node = Node,
                                     this_node = MyNode,
                                     socket = Ctrl,
                                     timer = Timer,
                                     this_flags = 0,
                                     other_version = ?ERL_DIST_VER_HIGH,
                                     request_type = Type},
    dist_util:handshake_we_started(HSData).

setopts(_Router, _Opts) -> ok.

getopts(_Router, _Opts) -> {ok, []}.

%% ------------------------------------------------------------------
%% Router: the endpoint JavaScript sends to, which dispatches the events of
%% each connection to the processes handling it
%% ------------------------------------------------------------------

start_router() ->
    case whereis(?ROUTER) of
        undefined ->
            Self = self(),
            Pid = spawn(fun() ->
                            true = register(?ROUTER, self()),
                            Self ! {?ROUTER, started},
                            router_init()
                        end),
            receive {?ROUTER, started} -> Pid end;
        Pid ->
            Pid
    end.

router_init() ->
    process_flag(trap_exit, true),
    router(#{acceptor => undefined, connections => #{}}).

router(State = #{connections := Connections}) ->
    receive
        {acceptor, Pid} ->
            router(State#{acceptor := Pid});

        {wasm, Event} ->
            router(incoming(Event, State));

        %% A connection's data goes to its reader
        {Ref, From, {route, Connection, Ctrl, Reader}} ->
            From ! {Ref, ok},
            router(watch(Connection, Ctrl, Reader, State));

        %% The connection is down, which the other end learns from JavaScript.
        %% The reader is linked to the controller, unless the controller died
        %% before it could link.
        {'DOWN', Ref, process, _Pid, _Reason} ->
            case [C || C := #{ref := R} <- Connections, R =:= Ref] of
                [Connection] ->
                    #{Connection := #{reader := Reader}} = Connections,
                    exit(Reader, connection_closed),
                    to_js(#{type => disconnect, connection => Connection}),
                    router(State#{connections := maps:remove(Connection, Connections)});
                [] ->
                    router(State)
            end;

        _Other ->
            router(State)
    end.

%% Events from the transport
incoming(#{<<"type">> := <<"connect">>, <<"connection">> := Connection, <<"to">> := To},
         State = #{acceptor := Acceptor, connections := Connections})
  when is_binary(Connection), not is_map_key(Connection, Connections) ->
    case atom_to_binary(node()) of
        To when is_pid(Acceptor) ->
            {Ctrl, Reader} = spawn_connection(Connection),
            Acceptor ! {incoming, Ctrl},
            watch(Connection, Ctrl, Reader, State);
        %% A node that doesn't listen refuses at once, rather than when the
        %% connection setup times out
        To ->
            to_js(#{type => disconnect, connection => Connection}),
            State;
        _ ->
            State
    end;
incoming(#{<<"type">> := <<"message">>, <<"connection">> := Connection, <<"data">> := Data},
         State = #{connections := Connections}) when is_binary(Data) ->
    case Connections of
        #{Connection := #{reader := Reader}} -> Reader ! {?ROUTER, data, Data};
        #{} -> ok
    end,
    State;
incoming(#{<<"type">> := <<"disconnect">>, <<"connection">> := Connection},
         State = #{connections := Connections}) ->
    case Connections of
        #{Connection := #{ctrl := Ctrl, reader := Reader}} ->
            exit(Ctrl, connection_closed),
            exit(Reader, connection_closed),
            unwatch(Connection, State);
        #{} ->
            State
    end;
incoming(_Event, State) ->
    State.

watch(Connection, Ctrl, Reader, State = #{connections := Connections}) ->
    Entry = #{ctrl => Ctrl, reader => Reader, ref => monitor(process, Ctrl)},
    State#{connections := Connections#{Connection => Entry}}.

unwatch(Connection, State = #{connections := Connections}) ->
    case Connections of
        #{Connection := #{ref := Ref}} ->
            demonitor(Ref, [flush]),
            State#{connections := maps:remove(Connection, Connections)};
        #{} ->
            State
    end.

call_router(Request) ->
    call(whereis(?ROUTER), Request).

%% ------------------------------------------------------------------
%% Connection processes: the controller writes, and the reader reads, so
%% that neither waits for the other, as in gen_tcp_dist. erts lets only the
%% controller fetch outgoing data, and another process handle incoming data.
%% The reader gets all the connection's data, from the start: it serves
%% dist_util's reads during the handshake, then hands the data to the VM.
%% ------------------------------------------------------------------

spawn_connection(Connection) ->
    Reader = spawn_opt(?MODULE, reader_init, [], [{priority, max} | ?SPAWN_OPTS]),
    Ctrl = spawn_opt(?MODULE, controller_init, [Connection, Reader],
                     [{priority, max} | ?SPAWN_OPTS]),
    {Ctrl, Reader}.

controller_init(Connection, Reader) ->
    link(Reader),
    handshake_loop(#{connection => Connection, reader => Reader, supervisor => undefined}).

handshake_loop(State = #{connection := Connection, reader := Reader}) ->
    receive
        %% dist_util's packets during the handshake, which TCP distribution
        %% frames with 2-byte lengths, as with {packet, 2}
        {Ref, From, {send, Packet}} ->
            send_message(Connection, [<<(iolist_size(Packet)):16>>, Packet]),
            From ! {Ref, ok},
            handshake_loop(State);

        {Ref, From, reader} ->
            From ! {Ref, Reader},
            handshake_loop(State);

        {Ref, From, {supervisor, Pid}} ->
            From ! {Ref, link(Pid)},
            handshake_loop(State#{supervisor := Pid});

        {Ref, From, getll} ->
            From ! {Ref, {ok, self()}},
            handshake_loop(State);

        {Ref, From, {address, Node}} ->
            [_Name, Host] = string:split(atom_to_list(Node), "@"),
            From ! {Ref, (address())#net_address{address = Node, host = Host}},
            handshake_loop(State);

        {Ref, From, Nodeup} when Nodeup =:= pre_nodeup; Nodeup =:= post_nodeup ->
            From ! {Ref, ok},
            handshake_loop(State);

        %% The node has its name by now, even a dynamic one
        {Ref, From, {handshake_complete, _Node, DHandle}} ->
            From ! {Ref, ok},
            announce(node()),
            ok = erlang:dist_ctrl_input_handler(DHandle, Reader),
            Reader ! {handshake_complete, DHandle},
            %% As in gen_tcp_dist: no longer at max priority, as it sends all
            %% the data now
            process_flag(priority, normal),
            erlang:dist_ctrl_get_data_notification(DHandle),
            output_loop(DHandle, Connection)
    end.

output_loop(DHandle, Connection) ->
    receive
        dist_data ->
            try send_data(DHandle, Connection)
            catch _:_ -> death_row()
            end,
            output_loop(DHandle, Connection);
        %% The connection supervisor asks for a tick when nothing was sent lately
        tick ->
            send_message(Connection, packet(<<>>)),
            output_loop(DHandle, Connection);
        _Other ->
            output_loop(DHandle, Connection)
    end.

send_data(DHandle, Connection) ->
    case erlang:dist_ctrl_get_data(DHandle) of
        none ->
            erlang:dist_ctrl_get_data_notification(DHandle);
        Data ->
            send_message(Connection, packet(Data)),
            send_data(DHandle, Connection)
    end.

reader_init() ->
    handshake_reader(#{buffer => <<>>, waiting => undefined}).

%% dist_util asks for one packet at a time during the handshake
handshake_reader(State = #{buffer := Buffer}) ->
    receive
        {?ROUTER, data, Data} ->
            handshake_reader(deliver(State#{buffer := <<Buffer/binary, Data/binary>>}));

        {Ref, From, {recv, _Length, Timeout}} ->
            Timer = case Timeout of
                        infinity -> undefined;
                        _ -> erlang:send_after(Timeout, self(), {recv_timeout, Ref})
                    end,
            handshake_reader(deliver(State#{waiting := {Ref, From, Timer}}));

        {recv_timeout, Ref} ->
            case State of
                #{waiting := {Ref, From, _Timer}} ->
                    From ! {Ref, {error, timeout}},
                    handshake_reader(State#{waiting := undefined});
                #{} ->
                    handshake_reader(State)
            end;

        %% The rest of the data, after what it has, goes to the VM
        {handshake_complete, DHandle} ->
            process_flag(priority, normal),
            input_loop(DHandle, put_packets(DHandle, Buffer))
    end.

%% Only the packets dist_util asks for, with 2-byte lengths, as the ones after
%% the handshake have 4-byte lengths
deliver(State = #{waiting := {Ref, From, Timer},
                  buffer := <<Size:16, Packet:Size/binary, Rest/binary>>}) ->
    cancel_timer(Timer),
    From ! {Ref, {ok, binary_to_list(Packet)}},
    State#{waiting := undefined, buffer := Rest};
deliver(State) ->
    State.

cancel_timer(undefined) -> ok;
cancel_timer(Timer) -> erlang:cancel_timer(Timer).

input_loop(DHandle, Buffer) ->
    receive
        {?ROUTER, data, Data} ->
            input_loop(DHandle, put_packets(DHandle, <<Buffer/binary, Data/binary>>));
        _Other ->
            input_loop(DHandle, Buffer)
    end.

%% The whole packets, which TCP distribution frames with 4-byte lengths after
%% the handshake, as with {packet, 4}. Empty ones are ticks.
put_packets(DHandle, <<Size:32, Packet:Size/binary, Rest/binary>>) ->
    put_data(DHandle, Packet),
    put_packets(DHandle, Rest);
put_packets(_DHandle, Rest) ->
    Rest.

put_data(DHandle, Packet) ->
    try erlang:dist_ctrl_put_data(DHandle, Packet)
    catch _:_ -> death_row()
    end.

%% As in gen_tcp_dist: wait to be taken down with the connection supervisor
death_row() ->
    receive after 5000 -> exit(connection_closed) end.

%% ------------------------------------------------------------------
%% Helpers
%% ------------------------------------------------------------------

%% Reads go to the reader. Asking the controller for it also fails the
%% handshake at once, if the controller is gone.
hs_data(Ctrl) ->
    Reader = call_ctrl(Ctrl, reader),
    #hs_data{f_send = fun(C, Packet) -> call_ctrl(C, {send, Packet}) end,
             f_recv = fun(_C, Length, Timeout) -> call(Reader, {recv, Length, Timeout}) end,
             f_setopts_pre_nodeup = fun(C) -> call_ctrl(C, pre_nodeup) end,
             f_setopts_post_nodeup = fun(C) -> call_ctrl(C, post_nodeup) end,
             f_getll = fun(C) -> call_ctrl(C, getll) end,
             f_handshake_complete = fun(C, Node, DHandle) ->
                                            call_ctrl(C, {handshake_complete, Node, DHandle})
                                    end,
             f_address = fun(C, Node) -> call_ctrl(C, {address, Node}) end,
             mf_tick = fun(C) -> C ! tick end,
             mf_setopts = fun(_C, _Opts) -> ok end,
             mf_getopts = fun(_C, _Opts) -> {ok, []} end}.

call_ctrl(Ctrl, Request) ->
    call(Ctrl, Request).

call(Pid, Request) ->
    Ref = monitor(process, Pid),
    Pid ! {Ref, self(), Request},
    receive
        {Ref, Result} ->
            demonitor(Ref, [flush]),
            Result;
        {'DOWN', Ref, process, Pid, Reason} ->
            exit({popcorn_dist, Reason})
    end.

send_message(Connection, Data) ->
    to_js(#{type => message, connection => Connection, data => iolist_to_binary(Data)}).

%% A packet after the handshake, framed as TCP distribution does
packet(Data) ->
    [<<(iolist_size(Data)):32>>, Data].

to_js(Event) ->
    wasm:send(#{'_popcorn' => #{t => dist, payload => Event}}).

%% This instance's node name, or null when distribution stops
announce(Node) ->
    wasm:send(#{'_popcorn' => #{t => dist_node, payload => Node}}).

%% Random rather than sequential, so that a rebooted node doesn't reuse the
%% connections its peers still have
new_connection() ->
    binary:encode_hex(rand:bytes(16), lowercase).

%% As net_kernel's: not 0 to 3
creation() ->
    rand:uniform((1 bsl 32) - 5) + 3.
