-module(erl_eval).
-export([fun_data/1]).

% Patch reason: in AVM erlang:fun_info(F, env) isn't implemented
fun_data(_F) -> false.
