-module(elixir_module).
-include("elixir.hrl").
-define(counter_attr, {elixir, counter}).
-compile({popcorn_patch_private, compile/7}).

compile(Line, Module, ModuleAsCharlist, Block, Vars, Prune, E) ->
    File = ?key(E, file),
    popcorn_module:check_module_availability(Module, Line, E),
    % Patch reason: tracing doesn't work on AVM yet
    % elixir_env:trace(defmodule, E),

    CompilerModules = popcorn_module:compiler_modules(),
    {Tables, Ref} = popcorn_module:build(Module, Line, File, E),
    {DataSet, DataBag} = Tables,

    try
        popcorn_module:put_compiler_modules([Module | CompilerModules]),
        {Result, ModuleE, CallbackE} = popcorn_module:eval_form(
            Line, Module, DataBag, Block, Vars, Prune, E
        ),
        CheckerInfo = popcorn_module:checker_info(),

        {Binary, PersistedAttributes, Autoload} =
            elixir_erl_compiler:spawn(fun() ->
                PersistedAttributes = ets:lookup_element(DataBag, persisted_attributes, 2),
                Attributes = popcorn_module:attributes(DataSet, DataBag, PersistedAttributes),
                {AllDefinitions, Private} = elixir_def:fetch_definitions(Module, E),

                OnLoadAttribute = lists:keyfind(on_load, 1, Attributes),
                NewPrivate = popcorn_module:validate_on_load_attribute(
                    OnLoadAttribute, AllDefinitions, Private, Line, E
                ),

                DialyzerAttribute = lists:keyfind(dialyzer, 1, Attributes),
                popcorn_module:validate_dialyzer_attribute(DialyzerAttribute, AllDefinitions, Line, E),

                NifsAttribute = lists:keyfind(nifs, 1, Attributes),
                popcorn_module:validate_nifs_attribute(NifsAttribute, AllDefinitions, Line, E),

                Unreachable = elixir_locals:warn_unused_local(
                    Module, AllDefinitions, NewPrivate, E
                ),
                elixir_locals:ensure_no_undefined_local(Module, AllDefinitions, E),
                elixir_locals:ensure_no_import_conflict(Module, AllDefinitions, E),

                %% We stop tracking locals here to avoid race conditions in case after_load
                %% evaluates code in a separate process that may write to locals table.
                elixir_locals:stop({DataSet, DataBag}),
                popcorn_module:make_readonly(Module),

                (not elixir_config:is_bootstrap()) andalso
                    'Elixir.Module':'__check_attributes__'(E, DataSet, DataBag),

                RawCompileOpts = popcorn_module:bag_lookup_element(DataBag, {accumulate, compile}, 2),
                CompileOpts = popcorn_module:validate_compile_opts(
                    RawCompileOpts, AllDefinitions, Unreachable, Line, E
                ),
                UsesBehaviours = popcorn_module:bag_lookup_element(DataBag, {accumulate, behaviour}, 2),
                Impls = popcorn_module:bag_lookup_element(DataBag, impls, 2),

                % Patch reason: tracing doesn't work on AVM yet
                % AfterVerify = popcorn_module:bag_lookup_element(DataBag, {accumulate, after_verify}, 2),
                % [
                %     elixir_env:trace({remote_function, [], VerifyMod, VerifyFun, 1}, CallbackE)
                %  || {VerifyMod, VerifyFun} <- AfterVerify
                % ],
                AfterVerify = [],

                ModuleMap = #{
                    struct => popcorn_module:get_struct(DataSet),
                    module => Module,
                    line => Line,
                    file => File,
                    relative_file => elixir_utils:relative_to_cwd(File),
                    attributes => Attributes,
                    definitions => AllDefinitions,
                    unreachable => Unreachable,
                    after_verify => AfterVerify,
                    compile_opts => CompileOpts,
                    deprecated => popcorn_module:get_deprecated(DataBag),
                    defines_behaviour => popcorn_module:defines_behaviour(DataBag),
                    uses_behaviours => UsesBehaviours,
                    impls => Impls
                },

                case ets:member(DataSet, {elixir, taint}) of
                    true -> elixir_errors:compile_error(E);
                    false -> ok
                end,

                Binary = elixir_erl:compile(ModuleMap),
                Autoload = proplists:get_value(autoload, CompileOpts, true),
                popcorn_module:spawn_parallel_checker(CheckerInfo, Module, ModuleMap),
                {Binary, PersistedAttributes, Autoload}
            end),

        Autoload andalso
            code:load_binary(Module, popcorn_module:beam_location(ModuleAsCharlist), Binary),
        popcorn_module:put_compiler_modules(CompilerModules),
        popcorn_module:eval_callbacks(Line, DataBag, after_compile, [CallbackE, Binary], CallbackE),
        % elixir_env:trace({on_module, Binary, none}, ModuleE),
        % warn_unused_attributes(DataSet, DataBag, PersistedAttributes, E), %Patch reason - it uses ets:select and we don't currently need warnings.
        popcorn_module:make_module_available(Module, Binary),
        (CheckerInfo == undefined) andalso
            [
                VerifyMod:VerifyFun(Module)
             || {VerifyMod, VerifyFun} <- popcorn_module:bag_lookup_element(
                    DataBag, {accumulate, after_verify}, 2
                )
            ],
        {module, Module, Binary, Result}
    catch
        error:undef:Stacktrace ->
            case Stacktrace of
                [{Module, Fun, Args, _Info} | _] = Stack when is_list(Args) ->
                    popcorn_module:compile_undef(Module, Fun, length(Args), Stack);
                [{Module, Fun, Arity, _Info} | _] = Stack ->
                    popcorn_module:compile_undef(Module, Fun, Arity, Stack);
                Stack ->
                    erlang:raise(error, undef, Stack)
            end
    after
        popcorn_module:put_compiler_modules(CompilerModules),
        ets:delete(DataSet),
        ets:delete(DataBag),
        elixir_code_server:call({undefmodule, Ref})
    end.
