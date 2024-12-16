-module(elixir_module).
-include("elixir.hrl").
-define(counter_attr, {elixir, counter}).
-compile({flb_patch_private, attributes/3}).
-compile({flb_patch_private, compile/7}).

compile(Line, Module, ModuleAsCharlist, Block, Vars, Prune, E) ->
    File = ?key(E, file),
    flb_module:check_module_availability(Module, Line, E),
    elixir_env:trace(defmodule, E),

    CompilerModules = flb_module:compiler_modules(),
    {Tables, Ref} = flb_module:build(Module, Line, File, E),
    {DataSet, DataBag} = Tables,

    try
        flb_module:put_compiler_modules([Module | CompilerModules]),
        {Result, ModuleE, CallbackE} = flb_module:eval_form(
            Line, Module, DataBag, Block, Vars, Prune, E
        ),
        CheckerInfo = flb_module:checker_info(),

        {Binary, PersistedAttributes, Autoload} =
            elixir_erl_compiler:spawn(fun() ->
                PersistedAttributes = ets:lookup_element(DataBag, persisted_attributes, 2),
                Attributes = attributes(DataSet, DataBag, PersistedAttributes),
                {AllDefinitions, Private} = elixir_def:fetch_definitions(Module, E),

                OnLoadAttribute = lists:keyfind(on_load, 1, Attributes),
                NewPrivate = flb_module:validate_on_load_attribute(
                    OnLoadAttribute, AllDefinitions, Private, Line, E
                ),

                DialyzerAttribute = lists:keyfind(dialyzer, 1, Attributes),
                flb_module:validate_dialyzer_attribute(DialyzerAttribute, AllDefinitions, Line, E),

                NifsAttribute = lists:keyfind(nifs, 1, Attributes),
                flb_module:validate_nifs_attribute(NifsAttribute, AllDefinitions, Line, E),

                Unreachable = elixir_locals:warn_unused_local(
                    Module, AllDefinitions, NewPrivate, E
                ),
                elixir_locals:ensure_no_undefined_local(Module, AllDefinitions, E),
                elixir_locals:ensure_no_import_conflict(Module, AllDefinitions, E),

                %% We stop tracking locals here to avoid race conditions in case after_load
                %% evaluates code in a separate process that may write to locals table.
                elixir_locals:stop({DataSet, DataBag}),
                flb_module:make_readonly(Module),

                (not elixir_config:is_bootstrap()) andalso
                    'Elixir.Module':'__check_attributes__'(E, DataSet, DataBag),

                RawCompileOpts = flb_module:bag_lookup_element(DataBag, {accumulate, compile}, 2),
                CompileOpts = flb_module:validate_compile_opts(
                    RawCompileOpts, AllDefinitions, Unreachable, Line, E
                ),
                UsesBehaviours = flb_module:bag_lookup_element(DataBag, {accumulate, behaviour}, 2),
                Impls = flb_module:bag_lookup_element(DataBag, impls, 2),

                AfterVerify = flb_module:bag_lookup_element(DataBag, {accumulate, after_verify}, 2),
                [
                    elixir_env:trace({remote_function, [], VerifyMod, VerifyFun, 1}, CallbackE)
                 || {VerifyMod, VerifyFun} <- AfterVerify
                ],

                ModuleMap = #{
                    struct => flb_module:get_struct(DataSet),
                    module => Module,
                    line => Line,
                    file => File,
                    relative_file => elixir_utils:relative_to_cwd(File),
                    attributes => Attributes,
                    definitions => AllDefinitions,
                    unreachable => Unreachable,
                    after_verify => AfterVerify,
                    compile_opts => CompileOpts,
                    deprecated => flb_module:get_deprecated(DataBag),
                    defines_behaviour => flb_module:defines_behaviour(DataBag),
                    uses_behaviours => UsesBehaviours,
                    impls => Impls
                },

                case ets:member(DataSet, {elixir, taint}) of
                    true -> elixir_errors:compile_error(E);
                    false -> ok
                end,

                Binary = elixir_erl:compile(ModuleMap),
                Autoload = proplists:get_value(autoload, CompileOpts, true),
                flb_module:spawn_parallel_checker(CheckerInfo, Module, ModuleMap),
                {Binary, PersistedAttributes, Autoload}
            end),

        Autoload andalso
            code:load_binary(Module, flb_module:beam_location(ModuleAsCharlist), Binary),
        flb_module:put_compiler_modules(CompilerModules),
        flb_module:eval_callbacks(Line, DataBag, after_compile, [CallbackE, Binary], CallbackE),
        elixir_env:trace({on_module, Binary, none}, ModuleE),
        % warn_unused_attributes(DataSet, DataBag, PersistedAttributes, E), %Patch reason - it uses ets:select and we don't currently need warnings.
        flb_module:make_module_available(Module, Binary),
        (CheckerInfo == undefined) andalso
            [
                VerifyMod:VerifyFun(Module)
             || {VerifyMod, VerifyFun} <- flb_module:bag_lookup_element(
                    DataBag, {accumulate, after_verify}, 2
                )
            ],
        {module, Module, Binary, Result}
    catch
        error:undef:Stacktrace ->
            case Stacktrace of
                [{Module, Fun, Args, _Info} | _] = Stack when is_list(Args) ->
                    flb_module:compile_undef(Module, Fun, length(Args), Stack);
                [{Module, Fun, Arity, _Info} | _] = Stack ->
                    flb_module:compile_undef(Module, Fun, Arity, Stack);
                Stack ->
                    erlang:raise(error, undef, Stack)
            end
    after
        flb_module:put_compiler_modules(CompilerModules),
        ets:delete(DataSet),
        ets:delete(DataBag),
        elixir_code_server:call({undefmodule, Ref})
    end.
%% Add attributes handling to the form

%Patch reason: PersistedAttributes might not be a list
attributes(DataSet, DataBag, PersistedAttributes) when not is_list(PersistedAttributes) ->
    attributes(DataSet, DataBag, [PersistedAttributes]);
attributes(DataSet, DataBag, PersistedAttributes) ->
    Result = [
        {Key, flb_module:lookup_attribute(DataSet, DataBag, Key)}
     || Key <- PersistedAttributes
    ],
    Result.
