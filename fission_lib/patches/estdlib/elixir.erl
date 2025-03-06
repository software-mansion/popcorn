%% Main entry point for Elixir functions. All of those functions are
%% private to the Elixir compiler and reserved to be used by Elixir only.
-module(elixir).
-export([start/2,eval_external_handler/3]).
-define(elixir_eval_env, {elixir, eval_env}).

%% Patch reason: currently we need to disable encoding/endianees test
start(_Type, _Args) ->
    % OTP = parse_otp_release(),
    flb_module:preload_common_modules(),
    % set_stdio_and_stderr_to_binary_and_maybe_utf8(OTP),
    % check_file_encoding(utf8),

    % case init:get_argument(elixir_root) of
    %   {ok, [[Root]]} ->
    %     load_paths(OTP, [
    %       Root ++ "/eex/ebin",
    %       Root ++ "/ex_unit/ebin",
    %       Root ++ "/iex/ebin",
    %       Root ++ "/logger/ebin",
    %       Root ++ "/mix/ebin",
    %       Root ++ "/elixir/ebin"
    %     ]);
    %   _ ->
    %     ok
    % end,

    % case application:get_env(elixir, check_endianness, true) of
    %   true  -> check_endianness();
    %   false -> ok
    % end,

    % case application:get_env(elixir, ansi_enabled) of
    %   {ok, _} -> ok;
    %   undefined ->
    %     %% Remove prim_tty module check as well as checks from scripts on Erlang/OTP 26
    %     ANSIEnabled = erlang:module_loaded(prim_tty) andalso (prim_tty:isatty(stdout) == true),
    %     application:set_env(elixir, ansi_enabled, ANSIEnabled)
    % end,

    Tokenizer =
        case code:ensure_loaded('Elixir.String.Tokenizer') of
            {module, Mod} -> Mod;
            _ -> elixir_tokenizer
        end,

    URIConfig = [
        {{uri, <<"ftp">>}, 21},
        {{uri, <<"sftp">>}, 22},
        {{uri, <<"tftp">>}, 69},
        {{uri, <<"http">>}, 80},
        {{uri, <<"https">>}, 443},
        {{uri, <<"ldap">>}, 389},
        {{uri, <<"ws">>}, 80},
        {{uri, <<"wss">>}, 443}
    ],

    Config = [
        %% ARGV options
        {at_exit, []},
        {argv, []},
        {no_halt, false},

        %% Compiler options
        {docs, true},
        {ignore_already_consolidated, false},
        {ignore_module_conflict, false},
        {on_undefined_variable, raise},
        {parser_options, [{columns, true}]},
        {debug_info, true},
        {warnings_as_errors, false},
        {relative_paths, true},
        {no_warn_undefined, []},
        {tracers, []}
        | URIConfig
    ],

    elixir_config:static(#{bootstrap => false, identifier_tokenizer => Tokenizer}),
    Tab = elixir_config:new(Config),

    case elixir_sup:start_link() of
        {ok, Sup} ->
            {ok, Sup, Tab};
        {error, _Reason} = Error ->
            elixir_config:delete(Tab),
            Error
    end.

eval_external_handler(Ann, FunOrModFun, Args) ->
    Current = try
        erlang:error(stacktrace)
    catch
        error:stacktrace:S -> S
    end,

    try
    case FunOrModFun of
        {Mod, Fun} -> apply(Mod, Fun, Args);
        Fun -> apply(Fun, Args)
    end
    catch
    Kind:Reason:Stacktrace ->
        %% Take everything up to the Elixir module
        Pruned =
        lists:takewhile(fun
            ({elixir,_,_,_}) -> false;
            (_) -> true
        end, Stacktrace),

        Caller =
        lists:dropwhile(fun
            ({elixir,_,_,_}) -> false;
            (_) -> true
        end, Stacktrace),

        % Patch reason: AVM doesn't implement erlang:process_info(self(), current_stacktrace)
        %% Now we prune any shared code path from erl_eval
        % {current_stacktrace, Current} =
        %   erlang:process_info(self(), current_stacktrace),

        %% We need to make sure that we don't generate more
        %% frames than supported. So we do our best to drop
        %% from the Caller, but if the caller has no frames,
        %% we need to drop from Pruned.
        {DroppedCaller, ToDrop} =
        case Caller of
            [] -> {[], true};
            _ -> {lists:droplast(Caller), false}
        end,

        Reversed = flb_module:drop_common(lists:reverse(Current), lists:reverse(Pruned), ToDrop),

        %% Add file+line information at the bottom
        Bottom =
        case erlang:get(?elixir_eval_env) of
            #{file := File} ->
            [{elixir_eval, '__FILE__', 1,
                [{file, elixir_utils:characters_to_list(File)}, {line, erl_anno:line(Ann)}]}];

            _ ->
            []
        end,

        Custom = lists:reverse(Bottom ++ Reversed, DroppedCaller),
        erlang:raise(Kind, Reason, Custom)
    end.
