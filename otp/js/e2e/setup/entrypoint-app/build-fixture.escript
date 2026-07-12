#!/usr/bin/env escript

main([SrcDir, StageDir, OutTar]) ->
    EbinDir = filename:join([StageDir, "lib", "test_entrypoint", "ebin"]),
    ok = filelib:ensure_dir(filename:join(EbinDir, "keep")),
    {ok, _} =
        compile:file(filename:join(SrcDir, "test_entrypoint_app.erl"), [{outdir, EbinDir}, report]),
    {ok, _} =
        file:copy(
            filename:join(SrcDir, "test_entrypoint.app"),
            filename:join(EbinDir, "test_entrypoint.app")
        ),
    ok = erl_tar:create(OutTar, [{"lib/test_entrypoint/ebin", EbinDir}], [compressed]),
    ok.
