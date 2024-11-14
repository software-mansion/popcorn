defmodule FissionLib.CoreErlangUtils do
  @moduledoc false

  @doc """
  Parses a beam file into the Core Erlang AST.
  """
  def parse(path) do
    {:ok, {_module, [abstract_code: {_backend, abstract_code}]}} =
      :beam_lib.chunks(~c"#{path}", [:abstract_code])

    {:ok, _module, ast} = :compile.noenv_forms(abstract_code, [:to_core])
    ast
  end

  @doc """
  Compiles the Core Erlang AST into a beam file.
  For some reason, a serialized file fails to be parsed again,
  for mysterious reasons.
  """
  def serialize(ast, path) do
    {:ok, _module, beam} = :compile.noenv_forms(ast, [:from_core])
    File.write!(path, beam)
  end

  @doc """
  Merges two modules in the form of Core Erlang AST,
  by adding all the functions from `patch_ast` to the
  `orig_ast`. If a function exists in both modules, the
  version from `patch_ast` is taken. Returns a merged
  module (also Core Erlang AST).
  """
  def merge_modules(orig_ast, patch_ast) do
    {:c_module, _meta, module_spec, orig_exports, orig_specs, orig_body} = orig_ast
    {:c_module, _meta, _module, patch_exports, patch_specs, patch_body} = patch_ast
    orig_exports = MapSet.new(orig_exports, fn {:c_var, _meta, fa} -> fa end)
    patch_exports = MapSet.new(patch_exports, fn {:c_var, _meta, fa} -> fa end)

    orig_body =
      rename_funs(orig_body, %{
        prefix: "avmo",
        funs: MapSet.difference(orig_exports, patch_exports)
      })

    patch_body = rename_funs(patch_body, %{prefix: "avmp", funs: patch_exports})

    body = orig_body ++ patch_body

    exports =
      MapSet.union(orig_exports, patch_exports) |> Enum.sort() |> Enum.map(&{:c_var, [], &1})

    specs = orig_specs ++ patch_specs

    {:c_module, [], module_spec, exports, specs, body}
  end

  @doc """
  Hardcodes tracing by injecting code into a Core Erlang AST input.

  The injected code prints module, function and arity of each remote call.
  """
  def add_simple_tracing({:c_module, module_meta, module_spec, exports, specs, body}) do
    {:c_literal, _meta, module} = module_spec
    body = if module == :simple_trace, do: body, else: do_add_simple_tracing(body)
    {:c_module, module_meta, module_spec, exports, specs, body}
  end

  defp do_add_simple_tracing(
         {:c_call, call_meta, {:c_literal, mod_meta, mod} = mv, {:c_literal, fun_meta, _fun} = fv,
          args}
       )
       when mod != :erlang do
    {file, line} =
      Enum.reduce(call_meta, {~c"no_file", 0}, fn
        {:file, file}, {_file, line} when is_list(file) -> {file, line}
        {line, _column}, {file, _line} when is_integer(line) -> {file, line}
        _other, acc -> acc
      end)

    {:c_call, call_meta, {:c_literal, mod_meta, :simple_trace}, {:c_literal, fun_meta, :trace},
     [
       mv,
       fv,
       {:c_literal, fun_meta, file},
       {:c_literal, fun_meta, line} | do_add_simple_tracing(args)
     ]}
  end

  defp do_add_simple_tracing(ast), do: traverse(ast, &do_add_simple_tracing/1)

  @doc """
  Dumps the Core Erlang AST and Core Erlang code into files.
  """
  def debug(ast, name \\ "out") do
    File.write!("#{name}.ast.exs", inspect(ast, pretty: true, limit: :infinity))
    File.open("#{name}.core", [:write], &:beam_listing.module(&1, ast))
  end

  defp rename_funs({:c_var, meta, {function, arity} = fa} = ast, ctx)
       when is_atom(function) and is_integer(arity) do
    if fa in ctx.funs do
      ast
    else
      {:c_var, meta, {:"#{ctx.prefix}_#{function}", arity}}
    end
  end

  defp rename_funs({:function, {function, arity} = fa} = ast, ctx)
       when is_atom(function) and is_integer(arity) do
    if fa in ctx.funs do
      ast
    else
      {:function, {:"#{ctx.prefix}_#{function}", arity}}
    end
  end

  defp rename_funs(ast, ctx) do
    traverse(ast, &rename_funs(&1, ctx))
  end

  defp traverse(ast, fun) when is_tuple(ast) do
    ast |> Tuple.to_list() |> fun.() |> List.to_tuple()
  end

  defp traverse([h | t], fun) do
    [fun.(h) | fun.(t)]
  end

  defp traverse(ast, _fun) do
    ast
  end
end
