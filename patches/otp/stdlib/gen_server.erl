-module(gen_server).

-compile({popcorn_patch_private, client_stacktrace/1}).

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
