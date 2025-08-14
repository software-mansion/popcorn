-module(proc_lib).

-compile({popcorn_patch_private, get_process_messages/1}).

% Patch reason: messages key is not supported in process_info in AtomVM
get_process_messages(Pid) ->
    Depth = error_logger:get_format_depth(),
    case Pid =/= self() orelse Depth =:= unlimited of
        true ->
            % {messages, Messages} = get_process_info(Pid, messages),
            % Messages;
            [];
        false ->
            %% If there are more messages than Depth, garbage
            %% collection can sometimes be avoided by collecting just
            %% enough messages for the crash report. It is assumed the
            %% process is about to die anyway.
            popcorn_module:receive_messages(Depth)
    end.
