%% Popcorn can't spawn inet_gethost, which runs native host name lookups, so
%% names are only looked up in files, unless an inetrc file is configured.
%% It's not done with an inetrc file, as reading one needs the Erlang parser.
-module(inet_config).
-compile([{popcorn_patch_private, [{read_inetrc, 0}]}]).
-export([read_inetrc/0]).

read_inetrc() ->
    case {application:get_env(inetrc), os:getenv("ERL_INETRC")} of
        {undefined, false} -> {"popcorn", [{lookup, [file]}]};
        _ -> popcorn_module:read_inetrc()
    end.
