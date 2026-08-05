-module(rand).

-compile({popcorn_patch_private, default_seed/0}).

%% Patch reason: term_to_binary used in `erlang:phash2/2' patch
%% doesn't support pids, so we use `erlang:pid_to_list/1'
%% Patch reason: `erlang.system_time/0' is missing because
%% `native' time unit is unsupported
default_seed() ->
  {erlang:phash2([{node(), erlang:pid_to_list(self())}]),
   erlang:system_time(microsecond),
   erlang:unique_integer()}.

