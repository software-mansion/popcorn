-module(application).

-export([get_env/2, get_env/3, get_all_env/1, get_all_key/1, get_key/2,
         get_application/0, get_application/1, get_supervisor/1,
         set_env/3, set_env/4, unset_env/2, unset_env/3]).

% Patch reason: under the static boot (config :popcorn, static_boot: true) the
% application_controller and application_master modules are dropped from the
% bundle; application env lives in an ETS table with the same name and row
% shapes as the controller's (ac_tab, created by popcorn_app_env). These
% functions read the table directly, so they work in both boot modes:
% classically the rows are the ones the real controller inserted, statically
% the ones popcorn_app_env inserted. Branches that need the controller or a
% master keep delegating and are only ever reached in the classic mode (the
% rows they key on don't exist under the static boot).

get_env(Application, Key) ->
    case ets:lookup(ac_tab, {env, Application, Key}) of
        [{_, Val}] -> {ok, Val};
        [] -> undefined
    end.

get_env(Application, Key, Default) ->
    case ets:lookup(ac_tab, {env, Application, Key}) of
        [{_, Val}] -> Val;
        [] -> Default
    end.

get_all_env(Application) ->
    [{Key, Val} || [Key, Val] <- ets:match(ac_tab, {{env, Application, '$1'}, '$2'})].

% {loaded, App} rows exist only in the classic mode; statically every app
% reads as "not loaded", so Application.spec/1 returns nil instead of
% crashing on the missing controller.
get_all_key(Application) ->
    case ets:member(ac_tab, {loaded, Application}) of
        true -> application_controller:get_all_key(Application);
        false -> undefined
    end.

get_key(Application, Key) ->
    case ets:member(ac_tab, {loaded, Application}) of
        true -> application_controller:get_key(Application, Key);
        false -> undefined
    end.

% {application_master, App} rows exist only in the classic mode.
get_application() ->
    get_application(group_leader()).

get_application(Pid) when is_pid(Pid) ->
    case process_info(Pid, group_leader) of
        undefined -> undefined;
        {group_leader, Gl} -> app_by_master(Gl)
    end;
get_application(Module) when is_atom(Module) ->
    case ets:match(ac_tab, {{loaded, '$1'}, '_'}) of
        [] -> undefined;
        _ -> application_controller:get_application_module(Module)
    end.

get_supervisor(Application) when is_atom(Application) ->
    case ets:lookup(ac_tab, {popcorn_app_sup, Application}) of
        [{_, Sup}] ->
            {ok, Sup};
        [] ->
            case app_master(Application) of
                undefined ->
                    undefined;
                Master ->
                    case application_master:get_child(Master) of
                        {Root, _App} -> {ok, Root};
                        error -> undefined
                    end
            end
    end.

app_by_master(Master) ->
    case ets:match(ac_tab, {{application_master, '$1'}, Master}) of
        [[AppName]] -> {ok, AppName};
        _ -> undefined
    end.

app_master(Application) ->
    case ets:lookup(ac_tab, {application_master, Application}) of
        [{_, Master}] -> Master;
        [] -> undefined
    end.

% Under the static boot writes go straight to the table (there is no
% controller process to serialize them); classically they delegate to the
% controller as before.
set_env(Application, Key, Val) ->
    set_env(Application, Key, Val, []).

set_env(Application, Key, Val, infinity) ->
    set_env(Application, Key, Val, [{timeout, infinity}]);
set_env(Application, Key, Val, Timeout) when is_integer(Timeout), Timeout >= 0 ->
    set_env(Application, Key, Val, [{timeout, Timeout}]);
set_env(Application, Key, Val, Opts) when is_list(Opts) ->
    case static_boot() of
        true ->
            true = ets:insert(ac_tab, {{env, Application, Key}, Val}),
            ok;
        false ->
            application_controller:set_env(Application, Key, Val, Opts)
    end.

unset_env(Application, Key) ->
    unset_env(Application, Key, []).

unset_env(Application, Key, infinity) ->
    unset_env(Application, Key, [{timeout, infinity}]);
unset_env(Application, Key, Timeout) when is_integer(Timeout), Timeout >= 0 ->
    unset_env(Application, Key, [{timeout, Timeout}]);
unset_env(Application, Key, Opts) when is_list(Opts) ->
    case static_boot() of
        true ->
            true = ets:delete(ac_tab, {env, Application, Key}),
            ok;
        false ->
            application_controller:unset_env(Application, Key, Opts)
    end.

static_boot() ->
    ets:member(ac_tab, popcorn_static_boot).
