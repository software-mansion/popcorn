#!/usr/bin/env escript
%%! -noshell

%% Exports the stdlib applications that depend on the BEAM's private
%% APIs, and writes them to %% OUT_DIR/otp-apps.zst, as one zstd stream of:
%%
%%   #{otp_version => Version,
%%     apps => [{application, App, Props}],
%%     modules => [#{app => App, module => Module, forms => Forms,
%%                   options => Options, chunks => Chunks}]}
%%
%% The apps are the specs from their .app files. A module's Forms are its
%% abstract code, stripped from docs, specs, types and columns. Options
%% and Chunks allow recompiling the Forms to the same bytecode as original.
%%
%% Usage: export-otp-apps.escript OTP_ROOT OUT_DIR

-mode(compile).

-define(APPS, [kernel, stdlib, compiler, crypto, asn1]).

%% A runtime build without crypto has no crypto or asn1
-define(REQUIRED_APPS, [kernel, stdlib, compiler]).

main([OtpRoot, OutDir]) ->
    Version = otp_version(OtpRoot),
    Dirs = [{App, Dir} || App <- ?APPS, Dir <- [app_dir(OtpRoot, App)], Dir =/= none],
    Missing = [App || App <- ?APPS, not lists:keymember(App, 1, Dirs)],
    [fail("~s isn't built in ~s", [App, OtpRoot]) || App <- Missing, lists:member(App, ?REQUIRED_APPS)],
    [warn("~s isn't built, so it's left out", [App]) || App <- Missing],
    _ = file:del_dir_r(OutDir),
    ok = filelib:ensure_path(OutDir),
    Apps = [app_spec(App, Dir) || {App, Dir} <- Dirs],
    Modules = [export_module(App, Dir, Module) || {App, Dir} <- Dirs,
                                                  Module <- app_modules(App, Apps)],
    Export = #{otp_version => Version, apps => Apps, modules => Modules},
    % Deterministic, so that exporting the same tree again changes nothing. One
    % stream compresses what the modules share, like the headers' records.
    Compressed = zstd:compress(term_to_binary(Export, [deterministic]),
                               #{compressionLevel => 19, windowLog => 27,
                                 enableLongDistanceMatching => true}),
    ok = file:write_file(filename:join(OutDir, "otp-apps.zst"), Compressed),
    io:format("Exported ~b modules of ~s from OTP ~s to ~s~n",
              [length(Modules), lists:join(", ", [atom_to_list(A) || {A, _} <- Dirs]), Version, OutDir]);
main(_) ->
    io:format(standard_error, "Usage: export-otp-apps.escript OTP_ROOT OUT_DIR~n", []),
    halt(1).

%% A build tree has OTP_VERSION at its root, an installation in releases/
otp_version(Root) ->
    Candidates = [filename:join(Root, "OTP_VERSION") |
                  filelib:wildcard(filename:join([Root, "releases", "*", "OTP_VERSION"]))],
    case [F || F <- Candidates, filelib:is_regular(F)] of
        [File | _] ->
            {ok, Contents} = file:read_file(File),
            string:trim(Contents);
        [] ->
            fail("no OTP_VERSION in ~s", [Root])
    end.

%% lib/<app> in a build tree, lib/<app>-<vsn> in an installation, if it's built
app_dir(Root, App) ->
    Dirs = [filename:join([Root, "lib", App]) |
            filelib:wildcard(filename:join([Root, "lib", atom_to_list(App) ++ "-*"]))],
    case [D || D <- Dirs, filelib:is_regular(filename:join([D, "ebin", atom_to_list(App) ++ ".app"]))] of
        [Dir | _] -> Dir;
        [] -> none
    end.

app_spec(App, Dir) ->
    {ok, [{application, App, _Props} = Spec]} =
        file:consult(filename:join([Dir, "ebin", atom_to_list(App) ++ ".app"])),
    Spec.

app_modules(App, Apps) ->
    {application, App, Props} = lists:keyfind(App, 2, Apps),
    proplists:get_value(modules, Props).

export_module(App, Dir, Module) ->
    Beam = filename:join([Dir, "ebin", atom_to_list(Module) ++ ".beam"]),
    case beam_lib:chunks(Beam, [debug_info]) of
        {ok, {Module, [{debug_info, {debug_info_v1, erl_abstract_code, {Forms, Options}}}]}} ->
            Chunks = [{<<"Meta">>, Meta} || Meta <- [chunk(Beam, "Meta")], Meta =/= none],
            #{app => App,
              module => Module,
              forms => [code_only(without_columns(F)) || F <- Forms, not metadata(F)],
              options => [O || O <- Options, changes_code(O)],
              chunks => Chunks};
        _ ->
            fail("no abstract code in ~s", [Beam])
    end.

chunk(Beam, Name) ->
    case beam_lib:chunks(Beam, [Name]) of
        {ok, {_Module, [{Name, Chunk}]}} -> Chunk;
        {error, beam_lib, _} -> none
    end.

%% Attributes for documentation, typing and tools, which the code doesn't use
metadata({attribute, _, Name, _}) ->
    lists:member(Name, [spec, type, opaque, nominal, export_type, removed_type,
                        dialyzer, deprecated, deprecated_type, deprecated_callback,
                        removed, unsafe, author, doc, moduledoc]);
metadata(_) ->
    false.

%% Without the types, records lose their field types, and callbacks keep only
%% their names and arities, which their behaviour_info/1 and tree-shaking use
code_only({attribute, A, record, {Name, Fields}}) ->
    Untyped = [case F of {typed_record_field, Field, _Type} -> Field; _ -> F end || F <- Fields],
    {attribute, A, record, {Name, Untyped}};
code_only({attribute, A, callback, {{Name, Arity}, _Types}}) ->
    Term = {type, A, term, []},
    Any = {type, A, 'fun', [{type, A, product, lists:duplicate(Arity, Term)}, Term]},
    {attribute, A, callback, {{Name, Arity}, [Any]}};
code_only(Form) ->
    Form.

%% Beams keep lines, not columns
without_columns(Form) ->
    erl_parse:map_anno(fun(Anno) -> erl_anno:set_location(erl_anno:line(Anno), Anno) end, Form).

%% Preprocessing is done, warnings aren't needed, and debug info is added anyway
changes_code({Key, _}) when Key =:= d; Key =:= i; Key =:= cwd -> false;
changes_code({d, _, _}) -> false;
changes_code(debug_info) -> false;
changes_code(Option) when is_atom(Option) ->
    Name = atom_to_list(Option),
    not (lists:prefix("warn_", Name) orelse lists:prefix("nowarn_", Name));
changes_code(_) -> true.

warn(Format, Args) ->
    io:format(standard_error, "export-otp-apps: " ++ Format ++ "~n", Args).

fail(Format, Args) ->
    warn(Format, Args),
    halt(1).
