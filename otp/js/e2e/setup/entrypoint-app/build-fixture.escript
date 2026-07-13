#!/usr/bin/env escript

main([SrcDir]) ->
    EbinDir = filename:join([SrcDir, "_build", "dev", "lib", "test_entrypoint", "ebin"]),
    ok = filelib:ensure_dir(filename:join(EbinDir, "keep")),
    {ok, _} =
        compile:file(filename:join(SrcDir, "test_entrypoint_app.erl"), [{outdir, EbinDir}, report]),
    {ok, _} =
        file:copy(
            filename:join(SrcDir, "test_entrypoint.app"),
            filename:join(EbinDir, "test_entrypoint.app")
        ),
    ok.
