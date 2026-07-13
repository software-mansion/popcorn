%% Patch reason: AtomVM uses brutal_kill only O.O
-module(elixir_sup).
-behaviour(supervisor).
-export([init/1, start_link/0]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, ok).

init(ok) ->
    Workers = [
        {
            elixir_config,
            {elixir_config, start_link, []},

            % Restart  = permanent | transient | temporary
            permanent,
            % Shutdown = brutal_kill | int() >= 0 | infinity
            % Patched, oryginally 2000
            brutal_kill,
            % Type     = worker | supervisor
            worker,
            % Modules  = [Module] | dynamic
            [elixir_config]
        },

        {
            elixir_code_server,
            {elixir_code_server, start_link, []},

            % Restart  = permanent | transient | temporary
            permanent,
            % Shutdown = brutal_kill | int() >= 0 | infinity
            % Patched, oryginally 2000
            brutal_kill,
            % Type     = worker | supervisor
            worker,
            % Modules  = [Module] | dynamic
            [elixir_code_server]
        }
    ],

    {ok, {{one_for_one, 3, 10}, Workers}}.
