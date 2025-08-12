%% Elixir compiler front-end to the Erlang backend.
-module(elixir_compiler).

-include("elixir.hrl").
-compile({popcorn_patch_private, spawned_compile/3}).
-compile({popcorn_patch_private, dispatch/4}).
spawned_compile(ExExprs, CompilerOpts, #{line := Line, file := File} = E) ->
    {Vars, S} = elixir_erl_var:from_env(E),
    {ErlExprs, _} = elixir_erl_pass:translate(ExExprs, erl_anno:new(Line), S),

    Module = popcorn_module:retrieve_compiler_module(),
    Fun = popcorn_module:code_fun(?key(E, module)),
    Forms = popcorn_module:code_mod(Fun, ErlExprs, Line, File, Module, Vars),

    {Module, Binary} = elixir_erl_compiler:noenv_forms(Forms, File, [nowarn_nomatch | CompilerOpts]),

    code:load_binary(Module, "", Binary),
    % {Module, Fun, is_purgeable(Binary)}. %Patch reason:  returns  "null function or function signature mismatch", needs to be fixed.
    {Module, Fun, false}.

dispatch(Module, Fun, Args, Purgeable) ->
    Res = Module:Fun(Args),
    % code:delete(Module), Patch Reason: uses code server
    Purgeable andalso popcorn_module:return_compiler_module(Module),
    Res.
