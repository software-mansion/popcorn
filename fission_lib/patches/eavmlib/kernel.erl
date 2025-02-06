-module(kernel).

-export([start/2]).

% Patch reason: the kernel app spawns some OTP internal utils
% We need to check whether these utils are compatible with
% and applicable to AtomVM
start(_, []) ->
    {ok, spawn(fun mock_loop/0), []}.

mock_loop() ->
    receive
        Message ->
            console:print(io_lib:format("Kernel app received unsupported message: ~p\n", [Message]))
    end,
    mock_loop().
