-module(elixir_errors).
-export([file_warn/4]).


% Patch reason: workaround for a weird `Unexpected term. Term is: 2B` error
% from AtomVM GC, that is in this case triggered by more complex calls
% to `io_lib:format`
file_warn(Meta, File, Module, Desc) when is_list(Meta), is_binary(File) ->
  file_warn(Meta, #{file => File}, Module, Desc);
file_warn(Meta, E, _Module, Desc) when is_list(Meta) ->
  % Skip warnings during bootstrap, they will be reported during recompilation
  case elixir_config:is_bootstrap() of
    true -> ok;
    false ->
      {EnvPosition, EnvFile, EnvStacktrace} = flb_module:env_format(Meta, E),
      % Message = Module:format_error(Desc),
      Message = io_lib:format("~p", [Desc]),
      flb_module:emit_diagnostic(warning, EnvPosition, EnvFile, Message, EnvStacktrace, [{read_snippet, true} | Meta])
  end.
