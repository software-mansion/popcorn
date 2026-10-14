-module(test_gen_server).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2]).

init(Value) ->
    {ok, Value}.

handle_call(<<"get">>, _From, Value) ->
    {reply, Value, Value};
handle_call([<<"add">>, Amount], _From, Value) ->
    NewValue = Value + Amount,
    {reply, NewValue, NewValue};
handle_call([<<"echo">>, Payload], _From, Value) ->
    {reply, Payload, Value};
handle_call(<<"bad_keys">>, _From, Value) ->
    {reply, #{1 => first, <<"1">> => second}, Value};
handle_call(<<"complex_key">>, _From, Value) ->
    {reply, #{{a, b} => true}, Value};
handle_call(<<"improper">>, _From, Value) ->
    {reply, [1 | 2], Value};
handle_call(<<"submillisecond">>, _From, Value) ->
    {ok, DateTime} = 'Elixir.DateTime':from_unix(1, microsecond),
    {reply, DateTime, Value};
handle_call(<<"unsupported">>, _From, Value) ->
    {reply, fun() -> ok end, Value};
handle_call(<<"wait">>, _From, Value) ->
    receive
    after infinity ->
        {reply, Value, Value}
    end.

handle_cast([<<"add">>, Amount], Value) ->
    {noreply, Value + Amount}.
