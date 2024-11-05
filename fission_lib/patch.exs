File.rm_rf!("out/patched_beam")
File.mkdir_p!("out/patched_beam")

names =
  Path.wildcard("out/{avm,estd}_beam/*.beam")
  |> MapSet.new(&Path.basename(&1, ".beam"))

names
|> Task.async_stream(
  fn name ->
    paths =
      ["out/avm_beam/#{name}.beam", "out/estd_beam/#{name}.beam"]
      |> Enum.filter(&File.exists?/1)

    with [avm, estd] <- paths do
      CoreErlangUtils.merge_modules(estd, avm, "out/patched_beam")
    else
      [path] ->
        File.cp!(path, "out/patched_beam/#{name}.beam")
        CoreErlangUtils.add_dupa_tracing("out/patched_beam/#{name}.beam")
    end
  end,
  timeout: 30_000
)
|> Stream.run()
