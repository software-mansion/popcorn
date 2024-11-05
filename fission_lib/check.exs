:code.add_path(~c"out/patched_beam")

Path.wildcard("out/patched_beam/*.beam")
|> Enum.map(&Path.basename(&1, ".beam"))
|> Enum.each(fn name ->
  case Code.ensure_loaded(:"#{name}") do
    {:module, _module} -> :ok
    _other -> IO.puts("Error loading module #{name}")
  end
end)
