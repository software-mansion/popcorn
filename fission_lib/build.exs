libs = ~w(estdlib eavmlib exavmlib libdupatrace)

~w(avm_beam avm_yrl estd_beam)
|> Enum.map(&"out/#{&1}")
|> Enum.each(fn dir ->
  File.rm_rf(dir)
  File.mkdir_p!(dir)
end)

File.rm_rf!("out/avm_beam")
File.mkdir_p!("out/avm_beam")
File.mkdir_p!("out/avm_yrl")
File.mkdir_p!("out/estd_beam")

ex_paths = Enum.flat_map(libs, &Path.wildcard("libs/#{&1}/**/*.ex"))

Task.async_stream(ex_paths, fn file ->
  IO.puts("Compiling #{file}")
  {_out, 0} = System.shell("elixirc --ignore-module-conflict -o out/avm_beam #{file}")
end)
|> Stream.run()

yrl_paths = Enum.flat_map(libs, &Path.wildcard("libs/#{&1}/**/*.yrl"))

Task.async_stream(yrl_paths, fn file ->
  IO.puts("Compiling #{file}")
  {_out, 0} = System.shell("erlc -o out/avm_yrl #{file}")
end)
|> Stream.run()

erl_paths =
  Enum.flat_map(libs, &Path.wildcard("libs/#{&1}/**/*.erl")) ++ Path.wildcard("out/avm_yrl/*.erl")

Task.async_stream(erl_paths, fn file ->
  IO.puts("Compiling #{file}")
  {_out, 0} = System.shell("erlc +debug_info -o out/avm_beam #{file}")
end)
|> Stream.run()

otp_beam_paths = Path.wildcard("estdlib_ebin/{compiler,erts,kernel,stdlib}*/**/*.beam")

Task.async_stream(otp_beam_paths, fn file ->
  File.cp!(file, Path.join("out/estd_beam", Path.basename(file)))
end)
|> Stream.run()

File.cp!("out/estd_beam/lists.beam", "out/estd_p_beam/lists.beam")

File.rm!("out/estd_beam/erlang.beam")
File.rm!("out/estd_beam/binary.beam")
