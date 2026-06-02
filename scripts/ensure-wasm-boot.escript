#!/usr/bin/env escript
%%! -noshell

main([SourceBoot, TargetBoot]) ->
    {ok, Binary} = file:read_file(SourceBoot),
    {script, Version, Entries} = binary_to_term(Binary),
    Patched = {script, Version, patch_entries(Entries)},
    ok = file:write_file(TargetBoot, term_to_binary(Patched));
main(_) ->
    io:format(standard_error, "Usage: ensure-wasm-boot.escript SOURCE_BOOT TARGET_BOOT~n", []),
    halt(1).

patch_entries(Entries) ->
    patch_stdlib_application(patch_stdlib_prim_load(Entries)).

patch_stdlib_prim_load([{path, ["$ROOT/lib/stdlib/ebin"]} = Path, {primLoad, Modules} | Rest]) ->
    [Path, {primLoad, add_module(wasm, Modules)} | Rest];
patch_stdlib_prim_load([Entry | Rest]) ->
    [Entry | patch_stdlib_prim_load(Rest)];
patch_stdlib_prim_load([]) ->
    [].

patch_stdlib_application([
    {apply, {application, load, [{application, stdlib, Properties}]}} | Rest
]) ->
    [
        {apply, {application, load, [{application, stdlib, patch_modules_property(Properties)}]}}
        | Rest
    ];
patch_stdlib_application([Entry | Rest]) ->
    [Entry | patch_stdlib_application(Rest)];
patch_stdlib_application([]) ->
    [].

patch_modules_property([{modules, Modules} | Rest]) ->
    [{modules, add_module(wasm, Modules)} | Rest];
patch_modules_property([Property | Rest]) ->
    [Property | patch_modules_property(Rest)];
patch_modules_property([]) ->
    [].

add_module(Module, Modules) ->
    case lists:member(Module, Modules) of
        true -> Modules;
        false -> Modules ++ [Module]
    end.
