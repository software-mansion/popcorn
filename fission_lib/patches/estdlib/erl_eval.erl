-module(erl_eval).

-compile({flb_patch_private, empty_fun_used_vars/0}).
empty_fun_used_vars() ->
    console:print("blah\n"),
    #{}.
