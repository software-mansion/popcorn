defmodule Popcorn.CoreErlangUtils do
  @moduledoc false

  @doc """
  Parses a beam binary into the Core Erlang AST.
  """
  def parse(beam) do
    {:ok, {_module, [abstract_code: {_backend, abstract_code}]}} =
      :beam_lib.chunks(beam, [:abstract_code])

    {:ok, _module, ast} = :compile.noenv_forms(abstract_code, [:to_core])
    ast
  end

  @doc """
  Compiles the Core Erlang AST into a beam binary.
  For some reason, the serialized beam fails to be parsed again,
  for mysterious reasons.
  """
  def serialize(ast) do
    {:ok, _module, beam} = :compile.noenv_forms(ast, [:from_core])
    beam
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
    {:c_module, _meta, _module_spec, patch_exports, patch_specs, patch_body} = patch_ast
    {:c_literal, _spec, module} = module_spec

    patch_private_overrides =
      patch_specs
      |> Enum.flat_map(fn
        {{:c_literal, _meta1, :compile}, {:c_literal, _meta2, params}} -> params
        _other -> []
      end)
      |> Enum.flat_map(fn
        {:popcorn_patch_private, funs} -> List.wrap(funs)
        _other -> []
      end)
      |> MapSet.new()

    orig_exports = MapSet.new(orig_exports, fn {:c_var, _meta, fa} -> fa end)

    patch_exports =
      patch_exports
      |> MapSet.new(fn {:c_var, _meta, fa} -> fa end)
      |> MapSet.difference(patch_private_overrides)
      |> MapSet.difference(MapSet.new(__info__: 1, module_info: 0, module_info: 1))

    exports = MapSet.union(orig_exports, patch_exports)

    orig_body =
      orig_body
      |> remove_funs(
        orig_exports
        |> MapSet.intersection(patch_exports)
        |> MapSet.union(patch_private_overrides)
      )
      |> rename_funs_and_local_calls(%{
        prefix: "avmo",
        except: MapSet.union(orig_exports, patch_private_overrides)
      })

    patch_body =
      patch_body
      |> rename_funs_and_local_calls(%{
        prefix: "avmp",
        except: MapSet.union(patch_exports, patch_private_overrides)
      })
      |> inject_private_calls(module, exports)

    body = patch_body ++ orig_body

    exports_ast = exports |> Enum.sort() |> Enum.map(&{:c_var, [], &1})

    specs = orig_specs ++ patch_specs

    {:c_module, [], module_spec, exports_ast, specs, body}
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
         {:c_call, call_meta, {:c_literal, mod_meta, mod} = mv, {:c_literal, fun_meta, fun} = fv,
          args}
       )
       when mod != :erlang or fun in [:nif_error, :error] do
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

  defp do_add_simple_tracing(
         {:c_apply, call_meta, {:c_var, fun_meta, {fun, _arity}}, args} = apply
       ) do
    if String.contains?("#{fun}", "$") or :compiler_generated in call_meta do
      apply
    else
      {file, line} =
        Enum.reduce(call_meta, {~c"no_file", 0}, fn
          {:file, file}, {_file, line} when is_list(file) -> {file, line}
          {line, _column}, {file, _line} when is_integer(line) -> {file, line}
          _other, acc -> acc
        end)

      trace_args = [
        {:c_literal, fun_meta, fun},
        {:c_literal, call_meta, length(args)},
        {:c_literal, call_meta, file},
        {:c_literal, call_meta, line}
      ]

      trace_apply =
        {:c_call, call_meta, {:c_literal, call_meta, :simple_trace},
         {:c_literal, call_meta, :trace_apply}, trace_args}

      {:c_seq, [], trace_apply, apply}
    end
  end

  defp do_add_simple_tracing(ast) do
    if is_tuple(ast) and tuple_size(ast) >= 2 and is_list(elem(ast, 1)) and
         :compiler_generated in elem(ast, 1) do
      ast
    else
      traverse(ast, &do_add_simple_tracing/1)
    end
  end

  @doc """
  Dumps the Core Erlang AST and Core Erlang code into files.
  """
  def debug(ast, name \\ "out") do
    File.write!("#{name}.ast.exs", inspect(ast, pretty: true, limit: :infinity))
    File.open("#{name}.core", [:write], &:beam_listing.module(&1, ast))
    ast
  end

  defp remove_funs(ast, funs) do
    Enum.reject(ast, fn
      {{:c_var, _var_meta, {fun, arity}}, {:c_fun, _fun_meta, _vars, _body}} ->
        {fun, arity} in funs

      _ast ->
        false
    end)
  end

  defp inject_private_calls(ast, module, exports) do
    with {:c_call, call_meta, mod_ast, fun_ast, args} <- ast,
         {:c_literal, mod_meta, :popcorn_module} <- mod_ast,
         {:c_literal, fun_meta, fun} <- fun_ast do
      arity = length(args)

      if {fun, arity} in exports do
        {:c_call, call_meta, {:c_literal, mod_meta, module}, {:c_literal, fun_meta, fun}, args}
      else
        {:c_apply, call_meta, {:c_var, fun_meta, {:"avmo_#{fun}", arity}}, args}
      end
    else
      _other -> traverse(ast, &inject_private_calls(&1, module, exports))
    end
  end

  defp rename_funs_and_local_calls({:c_var, meta, {function, arity} = fa} = ast, ctx)
       when is_atom(function) and is_integer(arity) do
    if fa in ctx.except do
      ast
    else
      {:c_var, meta, {:"#{ctx.prefix}_#{function}", arity}}
    end
  end

  defp rename_funs_and_local_calls({:function, {function, arity} = fa} = ast, ctx)
       when is_atom(function) and is_integer(arity) do
    if fa in ctx.except do
      ast
    else
      {:function, {:"#{ctx.prefix}_#{function}", arity}}
    end
  end

  defp rename_funs_and_local_calls({:id, {line, col, id}}, ctx) do
    "-" <> id = Atom.to_string(id)
    {:id, {line, col, :"-#{ctx.prefix}_#{id}"}}
  end

  defp rename_funs_and_local_calls(ast, ctx) do
    traverse(ast, &rename_funs_and_local_calls(&1, ctx))
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
