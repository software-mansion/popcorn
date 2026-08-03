-module(gen_server).

-compile({popcorn_patch_private, client_stacktrace/1}).

%% Patch reason: originally, client_stacktrace/1 calls
%% process_info(From, [registered_name, current_stacktrace])
%% and AtomVM doesn't support passing a list to process_info
%% and process_info(Pid, current_stacktrace) is mocked to return
%% an empty list. Thus, the patch just calls
%% process_info(From, registered_name) instead.
client_stacktrace(undefined) ->
    undefined;
client_stacktrace({From, _Tag}) ->
    client_stacktrace(From);
client_stacktrace(From) when is_pid(From), node(From) =:= node() ->
    case process_info(From, registered_name) of
        undefined ->
            {From, dead};
        [] ->
            {From, {From, []}};
        {registered_name, Name} ->
            {From, {Name, []}}
    end;
client_stacktrace(From) when is_pid(From) ->
    {From, remote}.
