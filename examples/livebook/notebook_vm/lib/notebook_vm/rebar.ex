defmodule NotebookVM.Rebar do
  @moduledoc """
  Compiles a rebar3 dependency in the VM, as `rebar3 bare compile` would.

  Mix compiles rebar3 dependencies by running rebar3, and the browser can't run
  OS commands. This covers what most Hex packages need: the Erlang sources in
  `src`, their Leex and Yecc files, and the `.app.src` file. Hooks, plugins and
  port compilers aren't supported.
  """

  @doc """
  Compiles the dependency in `source_dir`. Takes the environment Mix gives
  rebar3, and the paths of the dependencies' `ebin` directories, as a wildcard.
  Returns rebar3's exit status.
  """
  def compile(source_dir, env, paths) do
    out = Map.fetch!(env, "REBAR_BARE_COMPILER_OUTPUT_DIR")
    config = read_config(env["REBAR_CONFIG"])
    ebin = Path.join(out, "ebin")
    src = Path.join(source_dir, "src")
    File.mkdir_p!(ebin)

    # For parse transforms and include_lib
    Code.prepend_paths([ebin | Path.wildcard(paths)])

    with :ok <- generate_parsers(src),
         :ok <- compile_erl(source_dir, src, ebin, config),
         :ok <- write_app(source_dir, src, ebin) do
      0
    else
      {:error, message} ->
        Mix.shell().error(message)
        1
    end
  end

  defp read_config(nil), do: []

  defp read_config(path) do
    case :file.consult(path) do
      {:ok, terms} -> terms
      {:error, _reason} -> []
    end
  end

  defp generate_parsers(src) do
    results =
      Enum.map(Path.wildcard(Path.join(src, "**/*.xrl")), &:leex.file(to_charlist(&1))) ++
        Enum.map(Path.wildcard(Path.join(src, "**/*.yrl")), &:yecc.file(to_charlist(&1)))

    case Enum.find(results, &(not match?({:ok, _}, &1))) do
      nil -> :ok
      error -> {:error, "Could not generate a parser: #{inspect(error)}"}
    end
  end

  defp compile_erl(source_dir, src, ebin, config) do
    opts =
      [
        :return_errors,
        :report_warnings,
        :debug_info,
        {:outdir, to_charlist(ebin)},
        {:i, to_charlist(Path.join(source_dir, "include"))},
        {:i, to_charlist(src)}
      ] ++ erl_opts(config)

    first = for file <- Keyword.get(config, :erl_first_files, []), do: Path.join(source_dir, file)
    files = first ++ (Path.wildcard(Path.join(src, "**/*.erl")) -- first)

    Enum.reduce_while(files, :ok, fn file, :ok ->
      case :compile.file(to_charlist(file), opts) do
        {:ok, _module} -> {:cont, :ok}
        {:ok, _module, _warnings} -> {:cont, :ok}
        {:error, errors, _warnings} -> {:halt, {:error, format_errors(errors)}}
        :error -> {:halt, {:error, "Could not compile #{file}"}}
      end
    end)
  end

  # Newer Erlang/OTP versions warn about code that older ones accepted
  defp erl_opts(config) do
    config
    |> Keyword.get(:erl_opts, [])
    |> Enum.flat_map(fn
      :warnings_as_errors -> []
      {:platform_define, regex, name} -> platform_define(regex, [name])
      {:platform_define, regex, name, value} -> platform_define(regex, [name, value])
      opt -> [opt]
    end)
  end

  defp platform_define(regex, define) do
    arch = "#{System.otp_release()}-#{:erlang.system_info(:system_architecture)}"

    if Regex.match?(Regex.compile!(to_string(regex)), arch),
      do: [List.to_tuple([:d | define])],
      else: []
  end

  defp format_errors(errors) do
    for {file, file_errors} <- errors, {location, module, description} <- file_errors, into: "" do
      "#{file}:#{inspect(location)}: #{module.format_error(description)}\n"
    end
  end

  defp write_app(source_dir, src, ebin) do
    case Path.wildcard(Path.join(src, "*.app.src")) do
      [app_src] ->
        {:ok, [{:application, app, props}]} = :file.consult(app_src)

        modules =
          for beam <- Path.wildcard(Path.join(ebin, "*.beam")) do
            beam |> Path.basename(".beam") |> String.to_atom()
          end

        props =
          props
          |> Keyword.put(:modules, modules)
          |> Keyword.update(:vsn, ~c"0.0.0", &vsn(&1, source_dir))

        app_file = Path.join(ebin, "#{app}.app")
        File.write!(app_file, :io_lib.format(~c"~p.~n", [{:application, app, props}]))

      [] ->
        {:error, "No .app.src file in #{src}"}
    end
  end

  defp vsn(vsn, _source_dir) when is_list(vsn), do: vsn

  # Like rebar3's {vsn, git}: Hex packages have the version in their metadata
  defp vsn(_vsn, source_dir) do
    {:ok, metadata} = :file.consult(Path.join(source_dir, "hex_metadata.config"))
    metadata |> List.keyfind("version", 0) |> elem(1) |> to_charlist()
  end
end
