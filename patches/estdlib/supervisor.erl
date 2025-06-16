-module(supervisor).

-compile({popcorn_patch_private, init_children/2}).
-compile({popcorn_patch_private, add_restart/1}).

-type auto_shutdown() :: never | any_significant | all_significant.
-type child() :: undefined | pid().
-type child_id() :: term().
-type mfargs() :: {M :: module(), F :: atom(), A :: [term()] | undefined}.
-type modules() :: [module()] | dynamic.
-type restart() :: permanent | transient | temporary.
-type significant() :: boolean().
-type shutdown() :: brutal_kill | timeout().
-type worker() :: worker | supervisor.
-type strategy() :: one_for_all | one_for_one | rest_for_one | simple_one_for_one.
-type children() :: {Ids :: [child_id()], Db :: #{child_id() => child_rec()}}.

-record(child,
        {% pid is undefined when child is not running
         pid = undefined :: child() | {restarting, pid() | undefined} | [pid()],
         id :: child_id(),
         mfargs :: mfargs(),
         restart_type :: restart(),
         significant :: significant(),
         shutdown :: shutdown(),
         child_type :: worker(),
         modules = [] :: modules()}).

-type child_rec() :: #child{}.

-record(state,
        {name,
         strategy = one_for_one :: strategy(),
         children = {[], #{}} :: children(), % Ids in start order
         dynamics :: {maps, #{pid() => list()}} | {mapsets, #{pid() => []}} | undefined,
         intensity = 1 :: non_neg_integer(),
         period = 5 :: pos_integer(),
         restarts = [],
         dynamic_restarts = 0 :: non_neg_integer(),
         auto_shutdown = never :: auto_shutdown(),
         module,
         args}).

% Patch reason:
% no support for `hibernate` option
init_children(State, StartSpec) ->
  SupName = State#state.name,
  case popcorn_module:check_startspec(StartSpec, State#state.auto_shutdown) of
    {ok, Children} ->
      case popcorn_module:start_children(Children, SupName) of
        {ok, NChildren} ->
          %% Static supervisor are not expected to
          %% have much work to do so hibernate them
          %% to improve memory handling.
          %% PATCH: removed hibernate
          {ok, State#state{children = NChildren}};
        {error, NChildren, Reason} ->
          _ = popcorn_module:terminate_children(NChildren, SupName),
          {stop, {shutdown, Reason}}
      end;
    Error ->
      {stop, {start_spec, Error}}
  end.

% Patch reason: monotonic_time doesn't handle integer time units
add_restart(State) ->
  I = State#state.intensity,
  P = State#state.period,
  R = State#state.restarts,
  % Replaced code
  % Now = erlang:monotonic_time(1),
  Now = erlang:monotonic_time(second),
  R1 = add_restart(R, Now, P),
  State1 = State#state{restarts = R1},
  case length(R1) of
    CurI when CurI =< I ->
      {ok, State1};
    _ ->
      {terminate, State1}
  end.

add_restart(Restarts0, Now, Period) ->
  Treshold = Now - Period,
  Restarts1 = lists:takewhile(fun(R) -> R >= Treshold end, Restarts0),
  [Now | Restarts1].
