%% Patch reason: original set_file/2 crashes sometimes ¯\_(ツ)_/¯
%% for example in "spawn" test in eval_test.exs
%% See https://linear.app/swmansion/project/erl-annoset-file2-sometimes-crashes-in-erl-eval-76e146323242/overview

-module(erl_anno).

-export([set_file/2]).

set_file(_File, Anno) -> Anno.
