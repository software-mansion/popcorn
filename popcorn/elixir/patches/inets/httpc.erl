%% The browser allows no TCP connections, so in Popcorn's VM httpc's requests
%% go through Popcorn.Fetch.HTTPC, which makes them with the browser's fetch. The rest
%% of httpc and inets, such as profiles and options, works as in OTP.
-module(httpc).
-export([request/5, cancel_request/2, stream_next/1]).

request(Method, Request, HttpOptions, Options, Profile) ->
    case in_browser() of
        true -> 'Elixir.Popcorn.Fetch.HTTPC':request(Method, Request, HttpOptions, Options, Profile);
        false -> popcorn_module:request(Method, Request, HttpOptions, Options, Profile)
    end.

cancel_request(RequestId, Profile) ->
    case in_browser() of
        true -> 'Elixir.Popcorn.Fetch.HTTPC':cancel_request(RequestId, Profile);
        false -> popcorn_module:cancel_request(RequestId, Profile)
    end.

stream_next(Pid) ->
    case in_browser() of
        true -> 'Elixir.Popcorn.Fetch.HTTPC':stream_next(Pid);
        false -> popcorn_module:stream_next(Pid)
    end.

in_browser() ->
    erlang:system_info(system_architecture) =:= "wasm32-unknown-emscripten".
