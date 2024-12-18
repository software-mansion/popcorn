%% Main entry point for Elixir functions. All of those functions are
%% private to the Elixir compiler and reserved to be used by Elixir only.
-module(elixir).
-export([start/2]).

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
        {ignore_module_conflict, true},
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
