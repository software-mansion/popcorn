%% The browser allows no TCP connections, so httpc's requests go through the
%% browser's fetch, with Popcorn.Fetch: they follow the browser's rules, such as
%% CORS, and redirects are always followed. The rest of httpc and inets, such as
%% profiles and options, works as in OTP.
-module(httpc).
-export([request/5, cancel_request/2]).

-define(PART_SIZE, 65536).

request(Method, Request, HttpOptions, Options, Profile) ->
    case erlang:system_info(system_architecture) of
        "wasm32-unknown-emscripten" -> fetch_request(Method, Request, HttpOptions, Options);
        _ -> popcorn_module:request(Method, Request, HttpOptions, Options, Profile)
    end.

cancel_request(RequestId, Profile) ->
    case erase({?MODULE, RequestId}) of
        Pid when is_pid(Pid) ->
            exit(Pid, kill),
            ok;
        undefined ->
            popcorn_module:cancel_request(RequestId, Profile)
    end.

fetch_request(Method, Request, HttpOptions, Options) ->
    case fetch_args(Method, Request) of
        {ok, Args} ->
            Timeout = proplists:get_value(timeout, HttpOptions, infinity),
            case proplists:get_value(sync, Options, true) of
                true -> respond_sync(Args, Timeout, Options);
                false -> respond_async(Args, Timeout, Options)
            end;
        {error, _} = Error ->
            Error
    end.

fetch_args(Method, {Url, Headers}) ->
    {ok, #{method => method(Method), url => url(Url), headers => headers(Headers)}};
fetch_args(Method, {Url, Headers, ContentType, Body}) when is_binary(Body); is_list(Body) ->
    ContentTypeHeader = {<<"content-type">>, iolist_to_binary(ContentType)},
    {ok, #{
        method => method(Method),
        url => url(Url),
        headers => [ContentTypeHeader | headers(Headers)],
        body => iolist_to_binary(Body)
    }};
fetch_args(_Method, {_Url, _Headers, _ContentType, _Body}) ->
    {error, {not_supported, body_generator}};
fetch_args(_Method, Request) ->
    {error, {bad_request, Request}}.

method(Method) -> string:uppercase(atom_to_binary(Method)).

url(Url) -> unicode:characters_to_binary(Url).

headers(Headers) -> [{iolist_to_binary(Name), iolist_to_binary(Value)} || {Name, Value} <- Headers].

fetch(Args, Timeout) ->
    case 'Elixir.Popcorn.Fetch':request(Args, [{timeout, Timeout}]) of
        {ok, #{status := Status, headers := Headers, body := Body}} ->
            {ok, Status, response_headers(Headers), Body};
        {error, _} = Error ->
            Error
    end.

%% The browser has decoded the body, so its encoding and length don't apply
response_headers(Headers) ->
    [
        {binary_to_list(Name), binary_to_list(Value)}
     || {Name, Value} <- Headers,
        Name =/= <<"content-encoding">>,
        Name =/= <<"content-length">>
    ].

respond_sync(Args, Timeout, Options) ->
    case fetch(Args, Timeout) of
        {ok, Status, Headers, Body} ->
            case proplists:get_value(stream, Options, none) of
                File when is_list(File), (Status =:= 200 orelse Status =:= 206) ->
                    save(File, Body);
                _ ->
                    {ok, result(Status, Headers, Body, Options)}
            end;
        {error, _} = Error ->
            Error
    end.

%% Like httpc's handler processes, a process per request replies to the
%% receiver, and httpc:stream_next/1 casts to it
respond_async(Args, Timeout, Options) ->
    RequestId = make_ref(),
    Owner = self(),
    Deliver = receiver(proplists:get_value(receiver, Options, Owner)),
    Stream = proplists:get_value(stream, Options, none),
    forget_finished(),
    Pid = spawn(fun() ->
        Monitor = monitor(process, Owner),
        respond(RequestId, Args, Timeout, Options, Stream, Deliver, Monitor)
    end),
    put({?MODULE, RequestId}, Pid),
    {ok, RequestId}.

receiver(Pid) when is_pid(Pid) -> fun(Reply) -> Pid ! {http, Reply} end;
receiver(Fun) when is_function(Fun, 1) -> Fun;
receiver({Module, Function, Args}) -> fun(Reply) -> apply(Module, Function, [Reply | Args]) end.

%% The requests' processes, which cancel_request/2 finds, are kept until the next request
forget_finished() ->
    _ = [erase(Key) || {{?MODULE, _} = Key, Pid} <- get(), not is_process_alive(Pid)],
    ok.

respond(RequestId, Args, Timeout, Options, Stream, Deliver, Monitor) ->
    case fetch(Args, Timeout) of
        {ok, Status, Headers, Body} when Status =:= 200; Status =:= 206 ->
            respond_ok(RequestId, Status, Headers, Body, Options, Stream, Deliver, Monitor);
        {ok, Status, Headers, Body} ->
            Deliver({RequestId, result(Status, Headers, Body, Options)});
        {error, Reason} ->
            Deliver({RequestId, {error, Reason}})
    end.

%% httpc streams only 200 and 206 responses
respond_ok(RequestId, _Status, Headers, Body, _Options, self, Deliver, Monitor) ->
    Deliver({RequestId, stream_start, Headers}),
    stream(RequestId, Headers, Body, false, Deliver, Monitor);
respond_ok(RequestId, _Status, Headers, Body, _Options, {self, once}, Deliver, Monitor) ->
    Deliver({RequestId, stream_start, Headers, self()}),
    stream(RequestId, Headers, Body, true, Deliver, Monitor);
respond_ok(RequestId, _Status, _Headers, Body, _Options, File, Deliver, _Monitor) when is_list(File) ->
    case save(File, Body) of
        {ok, saved_to_file} -> Deliver({RequestId, saved_to_file});
        {error, _} = Error -> Deliver({RequestId, Error})
    end;
respond_ok(RequestId, Status, Headers, Body, Options, _Stream, Deliver, _Monitor) ->
    Deliver({RequestId, result(Status, Headers, Body, Options)}).

stream(RequestId, Headers, Body, Once, Deliver, Monitor) ->
    Once andalso await_next(Monitor),
    case Body of
        <<>> ->
            Deliver({RequestId, stream_end, Headers});
        <<Part:?PART_SIZE/binary, Rest/binary>> ->
            Deliver({RequestId, stream, Part}),
            stream(RequestId, Headers, Rest, Once, Deliver, Monitor);
        Part ->
            Deliver({RequestId, stream, Part}),
            stream(RequestId, Headers, <<>>, Once, Deliver, Monitor)
    end.

await_next(Monitor) ->
    receive
        {'$gen_cast', stream_next} -> true;
        {'DOWN', Monitor, process, _Pid, _Reason} -> exit(normal)
    end.

save(File, Body) ->
    case file:write_file(File, Body) of
        ok -> {ok, saved_to_file};
        {error, _} = Error -> Error
    end.

result(Status, Headers, Body, Options) ->
    FormattedBody =
        case proplists:get_value(body_format, Options, string) of
            binary -> Body;
            _ -> binary_to_list(Body)
        end,
    case proplists:get_value(full_result, Options, true) of
        true -> {{"HTTP/1.1", Status, httpd_util:reason_phrase(Status)}, Headers, FormattedBody};
        false -> {Status, FormattedBody}
    end.
