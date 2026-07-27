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
handle_call(<<"wait">>, _From, Value) ->
    receive
    after infinity ->
        {reply, Value, Value}
    end.

handle_cast([<<"add">>, Amount], Value) ->
    {noreply, Value + Amount}.
