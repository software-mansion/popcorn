-module(group_history).

-compile({popcorn_patch_private, init_running/0}).

-export([load/0]).

-spec load() -> [string()].
load() ->
    [].
%%    wait_for_kernel_safe_sup(),
%%    case history_status() of
%%        enabled ->
%%            %% If the size option were included the log file would be
%%            %% silently resized. If the log file does not exist, a
%%            %% {badarg, size} error is returned and a new log file
%%            %% created. If the file exists, the log file is resized if
%%            %% needed, with a size warning.
%%            try open_log_no_size() of
%%                {ok, ?LOG_NAME} ->
%%                    maybe_resize_log(?LOG_NAME);
%%                {repaired, ?LOG_NAME, {recovered, Good}, {badbytes, Bad}} ->
%%                    report_repairs(?LOG_NAME, Good, Bad),
%%                    read_full_log(?LOG_NAME);
%%                {error, {need_repair, _FileName}} ->
%%                    repair_log(?LOG_NAME);
%%                {error, {arg_mismatch, repair, true, false}} ->
%%                    repair_log(?LOG_NAME);
%%                {error, {name_already_open, _}} ->
%%                    show_rename_warning(),
%%                    read_full_log(?LOG_NAME);
%%                {error, {invalid_header, {vsn, Version}}} ->
%%                    upgrade_version(?LOG_NAME, Version),
%%                    load();
%%                {error, {badarg, size}} ->
%%                    try open_new_log(?LOG_NAME)
%%                    catch exit:_ ->
%%                            %% Same reason as comment in catch below
%%                            []
%%                    end;
%%                {error, Reason} ->
%%                    handle_open_error(Reason),
%%                    disable_history(),
%%                    []
%%            catch
%%                % disk_log shut down abruptly, possibly because
%%                % the node is shutting down. Ignore it.
%%                exit:_ -> []
%%            end;
%%        disabled ->
%%            [];
%%        Provider ->
%%            try Provider:load() of
%%                History when is_list(History) ->
%%                    History;
%%                Error ->
%%                    show_custom_provider_faulty_load_return(Provider, Error),
%%                    disable_history(),
%%                    []
%%            catch E:R:ST ->
%%                    show_custom_provider_crash(Provider, E, R, ST),
%%                    disable_history(),
%%                    []
%%            end
%%    end.

%%%%%%%%%%%%%%%
%%% PRIVATE %%%
%%%%%%%%%%%%%%%

%% Return if the system is running (not stopping)
-spec init_running() -> boolean().
init_running() ->
    true.
%%    case init:get_status() of
%%        {stopping, _} -> false;
%%        _ -> true
%%    end.
