%% Compiler backend to Erlang.

-module(elixir_erl).
-compile({popcorn_patch_private, docs_chunk/7}).
-compile({popcorn_patch_private, exports_md5_info/3}).
-compile({popcorn_patch_private, checker_chunk/1}).

docs_chunk(Set, Module, Line, Def, Defmacro, Types, Callbacks) ->
    case elixir_config:get(docs) of
        true ->
            {ModuleDocLine, ModuleDoc} = popcorn_module:get_moduledoc(Line, Set),
            ModuleDocMeta = popcorn_module:get_moduledoc_meta(Set),
            FunctionDocs = popcorn_module:get_docs(Set, Module, Def, function),
            MacroDocs = popcorn_module:get_docs(Set, Module, Defmacro, macro),
            CallbackDocs = popcorn_module:get_callback_docs(Set, Callbacks),
            TypeDocs = popcorn_module:get_type_docs(Set, Types),

            DocsChunkData = term_to_binary({
                docs_v1,
                erl_anno:new(ModuleDocLine),
                elixir,
                <<"text/markdown">>,
                ModuleDoc,
                ModuleDocMeta,
                FunctionDocs ++ MacroDocs ++ CallbackDocs ++ TypeDocs
                %Patch reason: usage of unsupported deterministic option
            }),

            [{<<"Docs">>, DocsChunkData}];
        false ->
            []
    end.

exports_md5_info(Struct, Def, Defmacro) ->
    %Patch reason: term_to_binary using unsupported deterministic option
    Md5 = erlang:md5(term_to_binary({Def, Defmacro, Struct})),
    {clause, 0, [{atom, 0, exports_md5}], [], [popcorn_module:elixir_to_erl(Md5)]}.

checker_chunk(#{
    definitions := Definitions, deprecated := Deprecated, defines_behaviour := DefinesBehaviour
}) ->
    DeprecatedMap = maps:from_list(Deprecated),

    Exports =
        lists:foldl(
            fun({Function, Kind, _Meta, _Clauses}, Acc) ->
                case Kind of
                    _ when Kind == def orelse Kind == defmacro ->
                        Reason = maps:get(Function, DeprecatedMap, nil),
                        [{Function, #{kind => Kind, deprecated_reason => Reason}} | Acc];
                    _ ->
                        Acc
                end
            end,
            [],
            Definitions
        ),

    Contents = #{
        exports => lists:sort(popcorn_module:behaviour_info_exports(DefinesBehaviour) ++ Exports)
    },

    %Patch reason: term_to_binary using unsupported deterministic option
    [{<<"ExCk">>, term_to_binary({elixir_checker_v1, Contents})}].
