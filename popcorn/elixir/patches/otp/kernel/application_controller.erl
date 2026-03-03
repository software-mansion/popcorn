-module(application_controller).

-export([handle_info/2]).

-compile({popcorn_patch_private, make_appl/1}).
-compile({popcorn_patch_private, init_starter/4}).

-import(lists, [zf/2, map/2, foreach/2, foldl/3,
		keyfind/3, keydelete/3, keyreplace/4]).

-record(state, {loading = [], starting = [], start_p_false = [], running = [],
		control = [], started = [], start_req = [], conf_data}).

-record(appl, {name, appl_data, descr, id, vsn, restart_type, inc_apps, opt_apps, apps}).

-define(AC, ?MODULE).

% Patch reason: in the `{'EXIT', Pid, Reason}` clause ets:match_delete
% (missing in AtomVM) is replaced with ets:delete. That clause is not
% only evaluated when the application dies (as the comment says),
% but also when any other linked process exits, so this patch is
% necessary not only for proper app termination, but even for its
% initialization
handle_info({ac_load_application_reply, AppName, Res}, S) ->
  case popcorn_module:keysearchdelete(AppName, 1, S#state.loading) of
{value, {_AppName, From}, Loading} ->
    gen_server:reply(From, Res),
    case Res of
  ok ->
      {noreply, S#state{loading = Loading}};
  {error, _R} ->
      NewS = popcorn_module:unload(AppName, S),
      {noreply, NewS#state{loading = Loading}}
    end;
false ->
    {noreply, S}
  end;

handle_info({ac_start_application_reply, AppName, Res}, S) ->
  Start_req = S#state.start_req,
  case lists:keyfind(AppName, 1, Starting = S#state.starting) of
{_AppName, RestartType, Type, From} ->
    case Res of
  start_it ->
      {true, Appl} = popcorn_module:get_loaded(AppName),
      popcorn_module:spawn_starter(From, Appl, S, Type),
      {noreply, S};
  {started, Node} ->
      popcorn_module:handle_application_started(AppName, 
               {ok, {distributed, Node}}, 
               S);
  not_started ->
      Started = S#state.started,
      Start_reqN =
    popcorn_module:reply_to_requester(AppName, Start_req, ok),
      {noreply, 
       S#state{starting = keydelete(AppName, 1, Starting),
         started = [{AppName, RestartType} | Started],
         start_req = Start_reqN}};
  {takeover, _Node} = Takeover ->
      {true, Appl} = popcorn_module:get_loaded(AppName),
      popcorn_module:spawn_starter(From, Appl, S, Takeover),
      NewStarting1 = keydelete(AppName, 1, Starting),
      NewStarting = [{AppName, RestartType, Takeover, From} | NewStarting1],
      {noreply, S#state{starting = NewStarting}};
  {error, Reason} = Error when RestartType =:= permanent ->
      Start_reqN = popcorn_module:reply_to_requester(AppName, Start_req, Error),
      {stop, popcorn_module:to_string(Reason), S#state{start_req = Start_reqN}};
  {error, _Reason} = Error ->
      Start_reqN = popcorn_module:reply_to_requester(AppName, Start_req, Error),
      {noreply, S#state{starting =
          keydelete(AppName, 1, Starting),
            start_req = Start_reqN}}
    end;
false ->
    {noreply, S} % someone called stop before control got that
  end;

handle_info({ac_change_application_req, AppName, Msg}, S) ->
  Running = S#state.running,
  Started = S#state.started,
  Starting = S#state.starting,
  case {keyfind(AppName, 1, Running), keyfind(AppName, 1, Started)} of
{{AppName, Id}, {_AppName2, Type}} ->
    case Msg of
  {started, Node} ->
      popcorn_module:stop_appl(AppName, Id, Type),
      NRunning = [{AppName, {distributed, Node}} |
      keydelete(AppName, 1, Running)],
      {noreply, S#state{running = NRunning}};
  {takeover, _Node, _RT} when is_pid(Id) -> % it is running already
      popcorn_module:notify_cntrl_started(AppName, Id, S, ok),
      {noreply, S};
  {takeover, Node, RT} ->
      NewS = popcorn_module:do_start(AppName, RT, {takeover, Node}, undefined, S),
      {noreply, NewS};
  {failover, _Node, _RT} when is_pid(Id) -> % it is running already
      popcorn_module:notify_cntrl_started(AppName, Id, S, ok),
      {noreply, S};
  {failover, Node, RT} ->
      case application:get_key(AppName, start_phases) of
    {ok, undefined} ->
        %% to be backwards compatible the application
        %% is not started as failover if start_phases  
        %% is not defined in the .app file
        NewS = popcorn_module:do_start(AppName, RT, normal, undefined, S),
        {noreply, NewS};
    {ok, _StartPhases} ->
        NewS = popcorn_module:do_start(AppName, RT, {failover, Node}, undefined, S),
        {noreply, NewS}
      end;
  stop_it ->
      popcorn_module:stop_appl(AppName, Id, Type),
      popcorn_module:cntrl(AppName, S, {ac_application_not_run, AppName}),
      NRunning = keyreplace(AppName, 1, Running, 
         {AppName, {distributed, []}}),
      {noreply, S#state{running = NRunning}};
  %% We should not try to start a running application!
  start_it when is_pid(Id) ->
      popcorn_module:notify_cntrl_started(AppName, Id, S, ok),
      {noreply, S};
  start_it ->
      NewS = popcorn_module:do_start(AppName, undefined, normal, undefined, S),
      {noreply, NewS};
  not_running ->
      NRunning = keydelete(AppName, 1, Running),
      {noreply, S#state{running = NRunning}};
  _ ->
      {noreply, S}
    end;
_ ->
    IsLoaded = popcorn_module:get_loaded(AppName),
    IsStarting = lists:keysearch(AppName, 1, Starting),
    IsStarted = lists:keysearch(AppName, 1, Started),
    IsRunning = lists:keysearch(AppName, 1, Running),

    case Msg of
  start_it ->
      case {IsLoaded, IsStarting, IsStarted, IsRunning} of
    %% already running
    {_, _, _, {value, _Tuple}} ->
        {noreply, S};
    %% not loaded
    {false, _, _, _} ->
        {noreply, S};
    %% only loaded
    {{true, _Appl}, false, false, false} ->
        {noreply, S};
    %% starting
    {{true, _Appl}, {value, Tuple}, false, false} ->
        {_AppName, _RStype, _Type, From} = Tuple,
        NewS = popcorn_module:do_start(AppName, undefined, normal, From, S),
        {noreply, NewS};
    %% started but not running
    {{true, _Appl}, _, {value, {AppName, _RestartType}}, false} ->
        NewS = popcorn_module:do_start(AppName, undefined, normal, undefined, S),
        SS = NewS#state{started = keydelete(AppName, 1, Started)},
        {noreply, SS}
      end;
  {started, Node} ->
      NRunning = [{AppName, {distributed, Node}} |
      keydelete(AppName, 1, Running)],
      {noreply, S#state{running = NRunning}};
  _ ->
      {noreply, S} % someone called stop before control got that
    end
  end;

%%-----------------------------------------------------------------
%% An application died.  Check its restart_type.  Maybe terminate
%% all other applications.
%%-----------------------------------------------------------------
handle_info({'EXIT', Pid, Reason}, S) ->
  case lists:keyfind(Pid, 2, S#state.running) of
  {AppName, _AmPid} ->
    ets:delete(ac_tab, {application_master, AppName}),
		NRunning = keydelete(Pid, 2, S#state.running),
		NewS = S#state{running = NRunning},
    popcorn_module:cntrl(AppName, S, {ac_application_stopped, AppName}),
    case lists:keyfind(AppName, 1, S#state.started) of
    {_AppName, temporary} ->
        popcorn_module:info_exited(AppName, Reason, temporary),
        {noreply, NewS};
    {_AppName, transient} when Reason =:= normal ->
        popcorn_module:info_exited(AppName, Reason, transient),
        {noreply, NewS};
    {_AppName, Type} ->
        popcorn_module:info_exited(AppName, Reason, Type),
        {stop, popcorn_module:to_string({application_terminated, AppName, Reason}), NewS}
      end;
  false ->
      {noreply, S#state{control = popcorn_module:del_cntrl(S#state.control, Pid)}}
    end;
  
handle_info(_, S) ->
  {noreply, S}.

%% Patch reason: we don't support code_server, nor dynamic application loading
%% This patch avoid calls to code_server and returns enoent right away
%% This ensures proper error is returned for missing optional applications
make_appl(Name) when is_atom(Name) ->
    FName = atom_to_list(Name) ++ ".app",
    {error, {file:format_error(enoent), FName}};

make_appl(Application) ->
    {ok, popcorn_module:make_appl_i(Application)}.


% Patch reason: originally this function calls `catch start_appl(Appl, S, Type)`.
% It triggers a bug in AtomVM that sometimes, undeterministically makes `start_appl`
% return the current process' state instead of what it actually returns.
% In this patch, start_appl is wrapped in a `try catch` equivalent of a `catch`,
% what prevents the bug from being triggered.
init_starter(_From, Appl, S, Type) ->
  process_flag(trap_exit, true),
  AppName = Appl#appl.name,
	Resp = try
    popcorn_module:start_appl(Appl, S, Type)
  catch
    throw:Reason -> Reason;
    _Error:Reason:Stacktrace -> {'EXIT', {Reason, Stacktrace}}
  end,
  gen_server:cast(?AC, {application_started, AppName, Resp}).
