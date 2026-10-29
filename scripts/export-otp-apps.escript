#!/usr/bin/env escript
%%! -noshell

%% Exports the sources of the OTP applications that Popcorn compiles instead of
%% packaging the host's, as they're tied to the runtime's emulator. Reads an OTP
%% build tree or installation and writes to OUT_DIR:
%%
%%   OTP_VERSION
%%   modules.eterm              {App, Module, CompileOptions} for each module
%%   lib/<app>/ebin/<app>.app
%%   lib/<app>/src/<module>.erl for the modules in the .app file
%%   lib/<app>/*/*.hrl          headers from src, include and the directories
%%                              the modules were compiled with
%%
%% The compile options are the ones each .beam was compiled with, with include
%% directories relative to OUT_DIR, so compiling the sources with them
%% reproduces the beams.
%%
%% Usage: export-otp-apps.escript OTP_ROOT OUT_DIR

-mode(compile).

-define(APPS, [kernel, stdlib, compiler, crypto, asn1]).

main([OtpRoot, OutDir]) ->
    ok = file:del_dir_r(OutDir) =:= ok orelse ok,
    ok = filelib:ensure_path(OutDir),
    Version = otp_version(OtpRoot),
    Apps = [{App, Dir} || App <- ?APPS, Dir <- [app_dir(OtpRoot, App)], Dir =/= none],
    [warn("~s isn't built, so it's left out", [App]) || App <- ?APPS, not lists:keymember(App, 1, Apps)],
    Modules = lists:append([export_app(App, Dir, OutDir) || {App, Dir} <- Apps]),
    IncludeDirs = lists:usort([Rel || {_App, _Module, Options} <- Modules, {i, Rel} <- Options] ++
                              [filename:join(["lib", App, Sub]) || {App, _} <- Apps, Sub <- ["src", "include"]]),
    [copy_headers(OtpRoot, Rel, OutDir) || Rel <- IncludeDirs],
    ok = file:write_file(filename:join(OutDir, "modules.eterm"),
                         [io_lib:format("~tp.~n", [Entry]) || Entry <- lists:sort(Modules)]),
    ok = file:write_file(filename:join(OutDir, "OTP_VERSION"), [Version, "\n"]),
    io:format("Exported ~b modules of ~s from OTP ~s to ~s~n",
              [length(Modules), lists:join(", ", [atom_to_list(A) || {A, _} <- Apps]), Version, OutDir]);
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

export_app(App, Dir, OutDir) ->
    AppFile = filename:join([Dir, "ebin", atom_to_list(App) ++ ".app"]),
    {ok, [{application, App, Props}]} = file:consult(AppFile),
    copy(AppFile, filename:join([OutDir, "lib", App, "ebin", filename:basename(AppFile)])),
    [export_module(App, Dir, Module, OutDir) || Module <- proplists:get_value(modules, Props)].

export_module(App, Dir, Module, OutDir) ->
    Name = atom_to_list(Module),
    Beam = filename:join([Dir, "ebin", Name ++ ".beam"]),
    Source = filename:join([Dir, "src", Name ++ ".erl"]),
    filelib:is_regular(Source) orelse fail("no source for ~s: ~s", [Module, Source]),
    {ok, {Module, [{compile_info, Info}]}} = beam_lib:chunks(Beam, [compile_info]),
    BuiltFrom = proplists:get_value(source, Info),
    Options = [relative_include(Option, BuiltFrom) || Option <- proplists:get_value(options, Info, [])],
    copy(Source, filename:join([OutDir, "lib", App, "src", Name ++ ".erl"])),
    {App, Module, Options}.

%% The beams were compiled in some OTP tree, maybe another machine's, so the
%% directories are made relative to it: .../lib/stdlib/src/../include becomes
%% lib/stdlib/include
relative_include({i, Dir}, BuiltFrom) ->
    Absolute = normalize(filename:absname(Dir, filename:dirname(BuiltFrom))),
    case string:find(Absolute, "/lib/", trailing) of
        nomatch ->
            fail("include directory outside of OTP's lib: ~s", [Absolute]);
        "/" ++ Rel ->
            [Lib, AppVsn | Rest] = filename:split(Rel),
            [AppName | _] = string:split(AppVsn, "-"),
            {i, filename:join([Lib, AppName | Rest])}
    end;
relative_include(Option, _BuiltFrom) ->
    Option.

normalize(Path) ->
    filename:join(lists:reverse(lists:foldl(fun
        (".", Acc) -> Acc;
        ("..", [_ | Acc]) -> Acc;
        (Part, Acc) -> [Part | Acc]
    end, [], filename:split(Path)))).

%% Copies a directory's headers, if the OTP tree has it
copy_headers(Root, Rel, OutDir) ->
    ["lib", AppName | Rest] = filename:split(Rel),
    case app_source_dir(Root, AppName) of
        none ->
            ok;
        AppDir ->
            Headers = filelib:wildcard(filename:join([AppDir | Rest] ++ ["*.hrl"])),
            [copy(H, filename:join([OutDir, Rel, filename:basename(H)])) || H <- Headers]
    end.

app_source_dir(Root, AppName) ->
    Dirs = [filename:join([Root, "lib", AppName]) |
            filelib:wildcard(filename:join([Root, "lib", AppName ++ "-*"]))],
    case [D || D <- Dirs, filelib:is_dir(D)] of
        [Dir | _] -> Dir;
        [] -> none
    end.

copy(From, To) ->
    ok = filelib:ensure_dir(To),
    {ok, _} = file:copy(From, To),
    ok.

warn(Format, Args) ->
    io:format(standard_error, "export-otp-apps: " ++ Format ++ "~n", Args).

fail(Format, Args) ->
    warn(Format, Args),
    halt(1).
