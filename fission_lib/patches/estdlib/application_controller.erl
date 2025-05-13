-module(application_controller).

-export([handle_info/2, make_appl/1]).
-compile({flb_patch_private, make_appl/1}).

-import(lists, [zf/2, map/2, foreach/2, foldl/3,
		keyfind/3, keydelete/3, keyreplace/4]).

-record(state, {loading = [], starting = [], start_p_false = [], running = [],
		control = [], started = [], start_req = [], conf_data}).

% Patch reason: in the `{'EXIT', Pid, Reason}` clause ets:match_delete
% (missing in AtomVM) is replaced with ets:delete. That clause is not
% only evaluated when the application dies (as the comment says),
% but also when any other linked process exits, so this patch is
% necessary not only for proper app termination, but even for its
% initialization
handle_info({ac_load_application_reply, AppName, Res}, S) ->
  case flb_module:keysearchdelete(AppName, 1, S#state.loading) of
{value, {_AppName, From}, Loading} ->
    gen_server:reply(From, Res),
    case Res of
  ok ->
      {noreply, S#state{loading = Loading}};
  {error, _R} ->
      NewS = flb_module:unload(AppName, S),
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
      {true, Appl} = flb_module:get_loaded(AppName),
      flb_module:spawn_starter(From, Appl, S, Type),
      {noreply, S};
  {started, Node} ->
      flb_module:handle_application_started(AppName, 
               {ok, {distributed, Node}}, 
               S);
  not_started ->
      Started = S#state.started,
      Start_reqN =
    flb_module:reply_to_requester(AppName, Start_req, ok),
      {noreply, 
       S#state{starting = keydelete(AppName, 1, Starting),
         started = [{AppName, RestartType} | Started],
         start_req = Start_reqN}};
  {takeover, _Node} = Takeover ->
      {true, Appl} = flb_module:get_loaded(AppName),
      flb_module:spawn_starter(From, Appl, S, Takeover),
      NewStarting1 = keydelete(AppName, 1, Starting),
      NewStarting = [{AppName, RestartType, Takeover, From} | NewStarting1],
      {noreply, S#state{starting = NewStarting}};
  {error, Reason} = Error when RestartType =:= permanent ->
      Start_reqN = flb_module:reply_to_requester(AppName, Start_req, Error),
      {stop, flb_module:to_string(Reason), S#state{start_req = Start_reqN}};
  {error, _Reason} = Error ->
      Start_reqN = flb_module:reply_to_requester(AppName, Start_req, Error),
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
      flb_module:stop_appl(AppName, Id, Type),
      NRunning = [{AppName, {distributed, Node}} |
      keydelete(AppName, 1, Running)],
      {noreply, S#state{running = NRunning}};
  {takeover, _Node, _RT} when is_pid(Id) -> % it is running already
      flb_module:notify_cntrl_started(AppName, Id, S, ok),
      {noreply, S};
  {takeover, Node, RT} ->
      NewS = flb_module:do_start(AppName, RT, {takeover, Node}, undefined, S),
      {noreply, NewS};
  {failover, _Node, _RT} when is_pid(Id) -> % it is running already
      flb_module:notify_cntrl_started(AppName, Id, S, ok),
      {noreply, S};
  {failover, Node, RT} ->
      case application:get_key(AppName, start_phases) of
    {ok, undefined} ->
        %% to be backwards compatible the application
        %% is not started as failover if start_phases  
        %% is not defined in the .app file
        NewS = flb_module:do_start(AppName, RT, normal, undefined, S),
        {noreply, NewS};
    {ok, _StartPhases} ->
        NewS = flb_module:do_start(AppName, RT, {failover, Node}, undefined, S),
        {noreply, NewS}
      end;
  stop_it ->
      flb_module:stop_appl(AppName, Id, Type),
      flb_module:cntrl(AppName, S, {ac_application_not_run, AppName}),
      NRunning = keyreplace(AppName, 1, Running, 
         {AppName, {distributed, []}}),
      {noreply, S#state{running = NRunning}};
  %% We should not try to start a running application!
  start_it when is_pid(Id) ->
      flb_module:notify_cntrl_started(AppName, Id, S, ok),
      {noreply, S};
  start_it ->
      NewS = flb_module:do_start(AppName, undefined, normal, undefined, S),
      {noreply, NewS};
  not_running ->
      NRunning = keydelete(AppName, 1, Running),
      {noreply, S#state{running = NRunning}};
  _ ->
      {noreply, S}
    end;
_ ->
    IsLoaded = flb_module:get_loaded(AppName),
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
        NewS = flb_module:do_start(AppName, undefined, normal, From, S),
        {noreply, NewS};
    %% started but not running
    {{true, _Appl}, _, {value, {AppName, _RestartType}}, false} ->
        NewS = flb_module:do_start(AppName, undefined, normal, undefined, S),
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
    flb_module:cntrl(AppName, S, {ac_application_stopped, AppName}),
    case lists:keyfind(AppName, 1, S#state.started) of
    {_AppName, temporary} ->
        flb_module:info_exited(AppName, Reason, temporary),
        {noreply, NewS};
    {_AppName, transient} when Reason =:= normal ->
        flb_module:info_exited(AppName, Reason, transient),
        {noreply, NewS};
    {_AppName, Type} ->
        flb_module:info_exited(AppName, Reason, Type),
        {stop, flb_module:to_string({application_terminated, AppName, Reason}), NewS}
      end;
  false ->
      {noreply, S#state{control = flb_module:del_cntrl(S#state.control, Pid)}}
    end;
  
handle_info(_, S) ->
  {noreply, S}.

% Patch reason: AtomVM doesn't support reading apps from .app files,
% so for now several apps are hardcoded
make_appl(Name) when is_atom(Name) ->
	{ok, flb_module:make_appl_i(get_hardcoded_app(Name))};

make_appl(Application) ->
    {ok, flb_module:make_appl_i(Application)}.


get_hardcoded_app(kernel) ->
    {application, kernel, [
        {description, "ERTS  CXC 138 10"},
        {vsn, "8.5.2"},
        {modules, [
            application,
            application_controller,
            application_master,
            application_starter,
            auth,
            code,
            code_server,
            dist_util,
            erl_boot_server,
            erl_compile_server,
            erl_distribution,
            erl_erts_errors,
            erl_reply,
            erl_kernel_errors,
            erl_signal_handler,
            erpc,
            error_handler,
            error_logger,
            file,
            file_server,
            file_io_server,
            global,
            global_group,
            global_search,
            group,
            group_history,
            heart,
            inet6_tcp,
            inet6_tcp_dist,
            inet6_udp,
            inet6_sctp,
            inet_config,
            inet_hosts,
            inet_gethost_native,
            inet_tcp_dist,
            kernel,
            kernel_config,
            kernel_refc,
            local_tcp,
            local_udp,
            logger,
            logger_backend,
            logger_config,
            logger_disk_log_h,
            logger_filters,
            logger_formatter,
            logger_h_common,
            logger_handler_watcher,
            logger_olp,
            logger_proxy,
            logger_server,
            logger_simple_h,
            logger_std_h,
            logger_sup,
            net,
            net_adm,
            net_kernel,
            os,
            ram_file,
            rpc,
            user_drv,
            user_sup,
            prim_tty,
            disk_log,
            disk_log_1,
            disk_log_server,
            disk_log_sup,
            dist_ac,
            erl_ddll,
            erl_epmd,
            erts_debug,
            gen_tcp,
            gen_tcp_socket,
            gen_udp,
            gen_udp_socket,
            gen_sctp,
            inet,
            inet_db,
            inet_dns,
            inet_parse,
            inet_res,
            inet_tcp,
            inet_udp,
            inet_sctp,
            pg,
            pg2,
            raw_file_io,
            raw_file_io_compressed,
            raw_file_io_deflate,
            raw_file_io_delayed,
            raw_file_io_inflate,
            raw_file_io_list,
            seq_trace,
            socket,
            standard_error,
            wrap_log_reader
        ]},
        {registered, [
            application_controller,
            erl_reply,
            auth,
            boot_server,
            code_server,
            disk_log_server,
            disk_log_sup,
            erl_prim_loader,
            error_logger,
            file_server_2,
            fixtable_server,
            global_group,
            global_name_server,
            heart,
            init,
            kernel_config,
            kernel_refc,
            kernel_sup,
            logger,
            logger_handler_watcher,
            logger_sup,
            net_kernel,
            net_sup,
            rex,
            user,
            os_server,
            ddll_server,
            erl_epmd,
            inet_db,
            pg
        ]},
        {applications, []},
        {env, [
            {logger_level, notice},
            {logger_sasl_compatible, false},
            {net_tickintensity, 4},
            {net_ticktime, 60},
            {prevent_overlapping_partitions, true},
            {shell_docs_ansi, auto}
        ]},
        {mod, {kernel, []}},
        {runtime_dependencies, [
            "erts-@OTP-18248@",
            "stdlib-@OTP-17932@",
            "sasl-3.0",
            "crypto-5.0"
        ]}
    ]};
get_hardcoded_app(stdlib) ->
    {application, stdlib, [
        {description, "ERTS  CXC 138 10"},
        {vsn, "4.2"},
        {modules, [
            array,
            base64,
            beam_lib,
            binary,
            c,
            calendar,
            dets,
            dets_server,
            dets_sup,
            dets_utils,
            dets_v9,
            dict,
            digraph,
            digraph_utils,
            edlin,
            edlin_context,
            edlin_expand,
            edlin_type_suggestion,
            epp,
            eval_bits,
            erl_abstract_code,
            erl_anno,
            erl_bits,
            erl_compile,
            erl_error,
            erl_eval,
            erl_expand_records,
            erl_features,
            erl_internal,
            erl_lint,
            erl_parse,
            erl_posix_msg,
            erl_pp,
            erl_scan,
            erl_stdlib_errors,
            erl_tar,
            error_logger_file_h,
            error_logger_tty_h,
            escript,
            ets,
            file_sorter,
            filelib,
            filename,
            gb_trees,
            gb_sets,
            gen,
            gen_event,
            gen_fsm,
            gen_server,
            gen_statem,
            io,
            io_lib,
            io_lib_format,
            io_lib_fread,
            io_lib_pretty,
            lists,
            log_mf_h,
            maps,
            math,
            ms_transform,
            orddict,
            ordsets,
            otp_internal,
            peer,
            pool,
            proc_lib,
            proplists,
            qlc,
            qlc_pt,
            queue,
            rand,
            random,
            re,
            sets,
            shell,
            shell_default,
            shell_docs,
            slave,
            sofs,
            string,
            supervisor,
            supervisor_bridge,
            sys,
            timer,
            unicode,
            unicode_util,
            uri_string,
            win32reg,
            zip
        ]},
        {registered, [
            timer_server,
            rsh_starter,
            take_over_monitor,
            pool_master,
            dets
        ]},
        {applications, [kernel]},
        {env, []},
        {runtime_dependencies, [
            "sasl-3.0",
            "kernel-@OTP-17932@",
            "erts-13.1",
            "crypto-4.5",
            "compiler-5.0"
        ]}
    ]};
get_hardcoded_app(compiler) ->
    {application, compiler, [
        {description, "ERTS  CXC 138 10"},
        {vsn, "8.2.2"},
        {modules, [
            beam_a,
            beam_asm,
            beam_bounds,
            beam_block,
            beam_call_types,
            beam_clean,
            beam_dict,
            beam_digraph,
            beam_disasm,
            beam_flatten,
            beam_jump,
            beam_kernel_to_ssa,
            beam_listing,
            beam_opcodes,
            beam_ssa,
            beam_ssa_bc_size,
            beam_ssa_bool,
            beam_ssa_bsm,
            beam_ssa_codegen,
            beam_ssa_dead,
            beam_ssa_lint,
            beam_ssa_opt,
            beam_ssa_pp,
            beam_ssa_pre_codegen,
            beam_ssa_recv,
            beam_ssa_share,
            beam_ssa_throw,
            beam_ssa_type,
            beam_trim,
            beam_types,
            beam_utils,
            beam_validator,
            beam_z,
            cerl,
            cerl_clauses,
            cerl_inline,
            cerl_trees,
            compile,
            core_scan,
            core_lint,
            core_parse,
            core_pp,
            core_lib,
            erl_bifs,
            rec_env,
            sys_core_alias,
            sys_core_bsm,
            sys_core_fold,
            sys_core_fold_lists,
            sys_core_inline,
            sys_core_prepare,
            sys_messages,
            sys_pre_attributes,
            v3_core,
            v3_kernel,
            v3_kernel_pp
        ]},
        {registered, []},
        {applications, [kernel, stdlib]},
        {env, []},
        {runtime_dependencies, [
            "stdlib-4.0",
            "kernel-8.4",
            "erts-13.0",
            "crypto-5.1"
        ]}
    ]};
get_hardcoded_app(elixir) ->
    {application, elixir, [
        {description, "elixir"},
        {vsn, "1.17.3"},
        {modules, [
            'Elixir.Access',
            'Elixir.Agent.Server',
            'Elixir.Agent',
            'Elixir.Application',
            'Elixir.ArgumentError',
            'Elixir.ArithmeticError',
            'Elixir.Atom',
            'Elixir.BadArityError',
            'Elixir.BadBooleanError',
            'Elixir.BadFunctionError',
            'Elixir.BadMapError',
            'Elixir.BadStructError',
            'Elixir.Base',
            'Elixir.Behaviour',
            'Elixir.Bitwise',
            'Elixir.Calendar.ISO',
            'Elixir.Calendar.TimeZoneDatabase',
            'Elixir.Calendar.UTCOnlyTimeZoneDatabase',
            'Elixir.Calendar',
            'Elixir.CaseClauseError',
            'Elixir.Code.Formatter',
            'Elixir.Code.Fragment',
            'Elixir.Code.Identifier',
            'Elixir.Code.LoadError',
            'Elixir.Code.Normalizer',
            'Elixir.Code.Typespec',
            'Elixir.Code',
            'Elixir.Collectable.BitString',
            'Elixir.Collectable.File.Stream',
            'Elixir.Collectable.HashDict',
            'Elixir.Collectable.HashSet',
            'Elixir.Collectable.IO.Stream',
            'Elixir.Collectable.List',
            'Elixir.Collectable.Map',
            'Elixir.Collectable.MapSet',
            'Elixir.Collectable',
            'Elixir.CompileError',
            'Elixir.CondClauseError',
            'Elixir.Config.Provider',
            'Elixir.Config.Reader',
            'Elixir.Config',
            'Elixir.Date.Range',
            'Elixir.Date',
            'Elixir.DateTime',
            'Elixir.Dict',
            'Elixir.Duration',
            'Elixir.DynamicSupervisor',
            'Elixir.Enum.EmptyError',
            'Elixir.Enum.OutOfBoundsError',
            'Elixir.Enum',
            'Elixir.Enumerable.Date.Range',
            'Elixir.Enumerable.File.Stream',
            'Elixir.Enumerable.Function',
            'Elixir.Enumerable.GenEvent.Stream',
            'Elixir.Enumerable.HashDict',
            'Elixir.Enumerable.HashSet',
            'Elixir.Enumerable.IO.Stream',
            'Elixir.Enumerable.List',
            'Elixir.Enumerable.Map',
            'Elixir.Enumerable.MapSet',
            'Elixir.Enumerable.Range',
            'Elixir.Enumerable.Stream',
            'Elixir.Enumerable',
            'Elixir.ErlangError',
            'Elixir.Exception',
            'Elixir.File.CopyError',
            'Elixir.File.Error',
            'Elixir.File.LinkError',
            'Elixir.File.RenameError',
            'Elixir.File.Stat',
            'Elixir.File.Stream',
            'Elixir.File',
            'Elixir.Float',
            'Elixir.Function',
            'Elixir.FunctionClauseError',
            'Elixir.GenEvent.Stream',
            'Elixir.GenEvent',
            'Elixir.GenServer',
            'Elixir.HashDict',
            'Elixir.HashSet',
            'Elixir.IO.ANSI.Docs',
            'Elixir.IO.ANSI.Sequence',
            'Elixir.IO.ANSI',
            'Elixir.IO.Stream',
            'Elixir.IO.StreamError',
            'Elixir.IO',
            'Elixir.Inspect.Algebra',
            'Elixir.Inspect.Any',
            'Elixir.Inspect.Atom',
            'Elixir.Inspect.BitString',
            'Elixir.Inspect.Date.Range',
            'Elixir.Inspect.Date',
            'Elixir.Inspect.DateTime',
            'Elixir.Inspect.Duration',
            'Elixir.Inspect.Error',
            'Elixir.Inspect.Float',
            'Elixir.Inspect.Function',
            'Elixir.Inspect.HashDict',
            'Elixir.Inspect.HashSet',
            'Elixir.Inspect.Inspect.Error',
            'Elixir.Inspect.Integer',
            'Elixir.Inspect.List',
            'Elixir.Inspect.Macro.Env',
            'Elixir.Inspect.Map',
            'Elixir.Inspect.MapSet',
            'Elixir.Inspect.NaiveDateTime',
            'Elixir.Inspect.Opts',
            'Elixir.Inspect.PID',
            'Elixir.Inspect.Port',
            'Elixir.Inspect.Range',
            'Elixir.Inspect.Reference',
            'Elixir.Inspect.Regex',
            'Elixir.Inspect.Stream',
            'Elixir.Inspect.Time',
            'Elixir.Inspect.Tuple',
            'Elixir.Inspect.URI',
            'Elixir.Inspect.Version.Requirement',
            'Elixir.Inspect.Version',
            'Elixir.Inspect',
            'Elixir.Integer',
            'Elixir.Kernel.CLI',
            'Elixir.Kernel.ErrorHandler',
            'Elixir.Kernel.LexicalTracker',
            'Elixir.Kernel.ParallelCompiler',
            'Elixir.Kernel.ParallelRequire',
            'Elixir.Kernel.SpecialForms',
            'Elixir.Kernel.Typespec',
            'Elixir.Kernel.TypespecError',
            'Elixir.Kernel.Utils',
            'Elixir.Kernel',
            'Elixir.KeyError',
            'Elixir.Keyword',
            'Elixir.List.Chars.Atom',
            'Elixir.List.Chars.BitString',
            'Elixir.List.Chars.Float',
            'Elixir.List.Chars.Integer',
            'Elixir.List.Chars.List',
            'Elixir.List.Chars',
            'Elixir.List',
            'Elixir.Macro.Env',
            'Elixir.Macro',
            'Elixir.Map',
            'Elixir.MapSet',
            'Elixir.MatchError',
            'Elixir.MismatchedDelimiterError',
            'Elixir.Module.Behaviour',
            'Elixir.Module.LocalsTracker',
            'Elixir.Module.ParallelChecker',
            'Elixir.Module.Types.Descr',
            'Elixir.Module.Types.Expr',
            'Elixir.Module.Types.Helpers',
            'Elixir.Module.Types.Of',
            'Elixir.Module.Types.Pattern',
            'Elixir.Module.Types',
            'Elixir.Module',
            'Elixir.NaiveDateTime',
            'Elixir.Node',
            'Elixir.OptionParser.ParseError',
            'Elixir.OptionParser',
            'Elixir.PartitionSupervisor',
            'Elixir.Path.Wildcard',
            'Elixir.Path',
            'Elixir.Port',
            'Elixir.Process',
            'Elixir.Protocol.UndefinedError',
            'Elixir.Protocol',
            'Elixir.Range',
            'Elixir.Record.Extractor',
            'Elixir.Record',
            'Elixir.Regex.CompileError',
            'Elixir.Regex',
            'Elixir.Registry.Partition',
            'Elixir.Registry.Supervisor',
            'Elixir.Registry',
            'Elixir.RuntimeError',
            'Elixir.Set',
            'Elixir.Stream.Reducers',
            'Elixir.Stream',
            'Elixir.String.Break',
            'Elixir.String.Chars.Atom',
            'Elixir.String.Chars.BitString',
            'Elixir.String.Chars.Date',
            'Elixir.String.Chars.DateTime',
            'Elixir.String.Chars.Float',
            'Elixir.String.Chars.Integer',
            'Elixir.String.Chars.List',
            'Elixir.String.Chars.NaiveDateTime',
            'Elixir.String.Chars.Time',
            'Elixir.String.Chars.URI',
            'Elixir.String.Chars.Version.Requirement',
            'Elixir.String.Chars.Version',
            'Elixir.String.Chars',
            'Elixir.String.Tokenizer.ScriptSet',
            'Elixir.String.Tokenizer.Security',
            'Elixir.String.Tokenizer',
            'Elixir.String.Unicode',
            'Elixir.String',
            'Elixir.StringIO',
            'Elixir.Supervisor.Default',
            'Elixir.Supervisor.Spec',
            'Elixir.Supervisor',
            'Elixir.SyntaxError',
            'Elixir.System.EnvError',
            'Elixir.System.SignalHandler',
            'Elixir.System',
            'Elixir.SystemLimitError',
            'Elixir.Task.Supervised',
            'Elixir.Task.Supervisor',
            'Elixir.Task',
            'Elixir.Time',
            'Elixir.TokenMissingError',
            'Elixir.TryClauseError',
            'Elixir.Tuple',
            'Elixir.URI.Error',
            'Elixir.URI',
            'Elixir.UndefinedFunctionError',
            'Elixir.UnicodeConversionError',
            'Elixir.Version.InvalidRequirementError',
            'Elixir.Version.InvalidVersionError',
            'Elixir.Version.Parser',
            'Elixir.Version.Requirement',
            'Elixir.Version',
            'Elixir.WithClauseError',
            elixir,
            elixir_aliases,
            elixir_bitstring,
            elixir_bootstrap,
            elixir_clauses,
            elixir_code_server,
            elixir_compiler,
            elixir_config,
            elixir_def,
            elixir_dispatch,
            elixir_env,
            elixir_erl,
            elixir_erl_clauses,
            elixir_erl_compiler,
            elixir_erl_for,
            elixir_erl_pass,
            elixir_erl_try,
            elixir_erl_var,
            elixir_errors,
            elixir_expand,
            elixir_fn,
            elixir_import,
            elixir_interpolation,
            elixir_lexical,
            elixir_locals,
            elixir_map,
            elixir_module,
            elixir_overridable,
            elixir_parser,
            elixir_quote,
            elixir_rewrite,
            elixir_sup,
            elixir_tokenizer,
            elixir_utils,
            iex
        ]},
        {registered, [elixir_sup, elixir_config, elixir_code_server]},
        {applications, [kernel, stdlib, compiler]},
        {mod, {elixir, []}},
        {env, [
            {ansi_syntax_colors, [
                {atom, cyan},
                {binary, default_color},
                {boolean, magenta},
                {charlist, yellow},
                {list, default_color},
                {map, default_color},
                {nil, magenta},
                {number, yellow},
                {string, green},
                {tuple, default_color},
                {variable, light_cyan},
                {call, default_color},
                {operator, default_color}
            ]},
            {check_endianness, true},
            {dbg_callback, {'Elixir.Macro', dbg, []}},
            {time_zone_database, 'Elixir.Calendar.UTCOnlyTimeZoneDatabase'}
        ]}
    ]};

get_hardcoded_app(eex) ->
    {application,eex,
             [{config_mtime,1726663337},
              {optional_applications,[]},
              {applications,[kernel,stdlib,elixir]},
              {description,"eex"},
              {modules,['Elixir.EEx','Elixir.EEx.Compiler',
                        'Elixir.EEx.Engine','Elixir.EEx.SmartEngine',
                        'Elixir.EEx.SyntaxError']},
              {registered,[]},
              {vsn,"1.17.3"}]};
get_hardcoded_app(Name) when is_atom(Name) ->
    erlang:raise(["Unknown app ", erlang:atom_to_list(Name)]).
