%% Purpose: Run the Erlang compiler.

-module(compile).

-compile({flb_patch_private, env_default_opts/0}).
-compile({flb_patch_private, debug_info_chunk/1}).
-type option() :: atom() | {atom(), term()} | {'d', atom(), term()}.
-type abstract_code() :: [erl_parse:abstract_form()].
-type error_info() :: erl_lint:error_info().
-type errors() :: [{file:filename(), [error_info()]}].
-type warnings() :: [{file:filename(), [error_info()]}].

-record(compile, {
    filename = "" :: file:filename(),
    dir = "" :: file:filename(),
    base = "" :: file:filename(),
    ifile = "" :: file:filename(),
    ofile = "" :: file:filename(),
    module = [] :: module() | [],
    %Abstract code for debugger.
    abstract_code = [] :: abstract_code(),
    %Options for compilation
    options = [] :: [option()],
    %Options for module_info
    mod_options = [] :: [option()],
    encoding = none :: none | epp:source_encoding(),
    errors = [] :: errors(),
    warnings = [] :: warnings(),
    extra_chunks = [] :: [{binary(), binary()}]
}).

%% Patch reason: os:getenv/1 is not supported.
env_default_opts() ->
    [].
% env_default_opts() ->
%     Key = "ERL_COMPILER_OPTIONS",
%     case os:getenv(Key) of
%         false ->
%             [];
%         Str when is_list(Str) ->
%             case erl_scan:string(Str) of
%                 {ok, Tokens, _} ->
%                     Dot = {dot, erl_anno:new(1)},
%                     case erl_parse:parse_term(Tokens ++ [Dot]) of
%                         {ok, List} when is_list(List) -> List;
%                         {ok, Term} ->
%                             [Term];
%                         {error, _Reason} ->
%                             io:format("Ignoring bad term in ~s\n", [Key]),
%                             []
%                     end;
%                 {error, {_, _, _Reason}, _} ->
%                     io:format("Ignoring bad term in ~s\n", [Key]),
%                     []
%             end
%     end.
%% Patch reason: end

debug_info_chunk(#compile{
    mod_options = ModOpts0,
    options = CompOpts,
    abstract_code = Abst
}) ->
    AbstOpts = flb_module:cleanup_compile_options(ModOpts0),
    {Backend, Metadata, ModOpts} =
        case proplists:get_value(debug_info, CompOpts, false) of
            {OptBackend, OptMetadata} when is_atom(OptBackend) ->
                ModOpts1 = proplists:delete(debug_info, ModOpts0),
                {OptBackend, OptMetadata, ModOpts1};
            true ->
                ModOpts1 = proplists:delete(debug_info, ModOpts0),
                {erl_abstract_code, {Abst, AbstOpts}, [debug_info | ModOpts1]};
            false ->
                {erl_abstract_code, {none, AbstOpts}, ModOpts0}
        end,
    %% Patch reason: term_to_binary/2 doesn't support the compressed option.
    % DebugInfo = erlang:term_to_binary(
    %     {debug_info_v1, Backend, Metadata},
    %     [compressed]
    % ),
    DebugInfo = erlang:term_to_binary(
        {debug_info_v1, Backend, Metadata}
        % [compressed]
    ),
    %% Patch reason: end
    {DebugInfo, ModOpts}.
