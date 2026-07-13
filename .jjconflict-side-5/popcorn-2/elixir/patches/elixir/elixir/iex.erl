-module(iex).
-export([start/2, shell/0]).

%% Patch reason: Module init is not supported in AtomVM
%% io:setopts/1 is not supported in AtomVM
start(Opts, MFA) ->
  {ok, _} = application:ensure_all_started(elixir),
  {ok, _} = application:ensure_all_started(iex),

  spawn(fun() ->
%%    case init:notify_when_started(self()) of
%%      started -> ok;
%%      _ -> init:wait_until_started()
%%    end,

%%    ok = io:setopts([{binary, true}, {encoding, unicode}]),
    'Elixir.IEx.Server':run_from_shell(Opts, MFA)
  end).

%% Patch reason: Module init is not supported in AtomVM
shell() ->
%%  Args = init:get_plain_arguments(),
  Args = [],
  case popcorn_module:get_remsh(Args) of
    nil ->
      popcorn_module:start_mfa(Args, {elixir, start_cli, []});

    Remote ->
      Ref = make_ref(),

      Parent =
        spawn_link(fun() ->
          receive
            {'begin', Ref, Other} ->
              elixir:start_cli(),
              Other ! {done, Ref}
          end
        end),

      {remote, Remote, popcorn_module:start_mfa(Args, {?MODULE, sync_remote, [Parent, Ref]})}
  end.
