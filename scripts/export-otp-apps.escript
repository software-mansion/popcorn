#!/usr/bin/env escript
%%! -noshell

%% Exports the OTP applications that Popcorn compiles instead of packaging the
%% host's, as they're tied to the runtime's emulator. Reads the beams of an OTP
%% build tree or installation, which have debug info, and writes them to
%% OUT_DIR/otp-apps.zst, as one zstd stream of:
%%
%%   #{otp_version => Version,
%%     apps => [{application, App, Props}],
%%     modules => [{App, Module, Forms, Options, Docs, Chunks}]}
%%
%% The apps are the specs from their .app files. A module's Forms are its
%% abstract code, which is already preprocessed, without specs and columns:
%% neither changes the code. Options are the ones its beam was compiled with
%% that change the code, Docs its decoded Docs chunk, if it has one, and Chunks
%% the other chunks the compiler makes from what the forms lack: Meta, with the
%% features enabled while preprocessing. Compiling the forms with them
%% reproduces the beam's code and lines, and adding the chunks, with the docs
%% encoded as the compiler does, its Docs and Meta chunks.
%%
%% Usage: export-otp-apps.escript OTP_ROOT OUT_DIR

-mode(compile).

-define(APPS, [kernel, stdlib, compiler, crypto, asn1]).

main([OtpRoot, OutDir]) ->
    _ = file:del_dir_r(OutDir),
    ok = filelib:ensure_path(OutDir),
    Version = otp_version(OtpRoot),
    Dirs = [{App, Dir} || App <- ?APPS, Dir <- [app_dir(OtpRoot, App)], Dir =/= none],
    [warn("~s isn't built, so it's left out", [App]) || App <- ?APPS, not lists:keymember(App, 1, Dirs)],
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
            % Decoded, so that the stream compresses it
            Docs = case chunk(Beam, "Docs") of
                       none -> none;
                       Chunk -> binary_to_term(Chunk)
                   end,
            Chunks = [{<<"Meta">>, Meta} || Meta <- [chunk(Beam, "Meta")], Meta =/= none],
            {App, Module, [without_columns(F) || F <- Forms, not spec(F)],
             [O || O <- Options, changes_code(O)], Docs, Chunks};
        _ ->
            fail("no abstract code in ~s", [Beam])
    end.

chunk(Beam, Name) ->
    case beam_lib:chunks(Beam, [Name]) of
        {ok, {_Module, [{Name, Chunk}]}} -> Chunk;
        {error, beam_lib, _} -> none
    end.

spec({attribute, _, spec, _}) -> true;
spec(_) -> false.

%% Beams keep lines, not columns, except in nominal types, which are attributes
without_columns({attribute, _, nominal, _} = Form) ->
    Form;
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
