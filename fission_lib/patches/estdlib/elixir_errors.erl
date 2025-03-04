-module(elixir_errors).
-export([print_diagnostics/1, print_diagnostic/2, emit_diagnostic/6]).
-export([print_warning/3]).

% Patch reason: format_stacktrace_entry breaks on application controller calling ets:match.
print_error(_Meta, _Env, _Module, _Desc) -> ok.
print_diagnostics(_Diagnostics) -> ok.
print_diagnostic(Diagnostic, _ReadSnippet) -> Diagnostic.
emit_diagnostic(_Severity, _Position, _File, _Message, _Stacktrace, _Options) -> ok.
print_warning(_Position, _File, _Message) -> ok.
