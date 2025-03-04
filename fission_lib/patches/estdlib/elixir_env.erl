-module(elixir_env).
-include("elixir.hrl").
-export([check_unused_vars/2, merge_and_check_unused_vars/3]).

check_unused_vars(#elixir_ex{unused={Unused, _Version}}, E) ->
  % Patch reason: some NIF is unimplemented
  % [elixir_errors:file_warn(calculate_span(Meta, Name), E, ?MODULE, {unused_var, Name, Overridden}) ||
  %   {{{Name, _Kind}, _Count}, {Meta, Overridden}} <- maps:to_list(Unused), is_unused_var(Name)],
  E.

merge_and_check_unused_vars(S, #elixir_ex{vars={Read, Write}, unused={Unused, _Version}}, E) ->
  #elixir_ex{unused={ClauseUnused, Version}} = S,
  NewUnused = merge_and_check_unused_vars(Read, Unused, ClauseUnused, E),
  S#elixir_ex{unused={NewUnused, Version}, vars={Read, Write}}.

merge_and_check_unused_vars(Current, Unused, ClauseUnused, E) ->
  maps:fold(fun
    ({Var, Count} = Key, false, Acc) ->
      case Current of
        #{Var := CurrentCount} when Count =< CurrentCount ->
          %% The parent knows it, so we have to propagate it was used up.
          Acc#{Key => false};

        #{} ->
          Acc
      end;

    ({{Name, _Kind}, _Count}, {Meta, Overridden}, Acc) ->
      case is_unused_var(Name) of
        true ->
          % Patch reason: some NIF is unimplemented
          % Warn = {unused_var, Name, Overridden},
          % elixir_errors:file_warn(Meta, E, ?MODULE, Warn);
          ok;

        false ->
          ok
      end,

      Acc
  end, Unused, ClauseUnused).

is_unused_var(Name) ->
  case atom_to_list(Name) of
    "_" ++ Rest -> is_compiler_var(Rest);
    _ -> true
  end.

is_compiler_var([$_]) -> true;
is_compiler_var([Var | Rest]) when Var =:= $_; Var >= $A, Var =< $Z -> is_compiler_var(Rest);
is_compiler_var(_) -> false.
