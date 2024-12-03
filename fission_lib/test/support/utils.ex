defmodule Utils do
  def compile_quoted(code, tmp_dir) do
    tmp_dir = "#{tmp_dir}/_build#{:erlang.unique_integer()}"
    File.rm_rf!(tmp_dir)
    File.mkdir_p!(tmp_dir)
    File.write!("#{tmp_dir}/code.ex", Macro.to_string(code))
    {_output, 0} = System.shell("elixirc #{tmp_dir}/code.ex -o #{tmp_dir}")

    result =
      Path.wildcard("#{tmp_dir}/*.beam")
      |> Enum.map(&{String.to_atom(Path.basename(&1, ".beam")), File.read!(&1)})

    File.rm_rf!(tmp_dir)
    result
  end
end
