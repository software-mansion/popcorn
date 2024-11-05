defmodule CoreErlangUtils do
  def merge_modules(orig_path, patch_path, out_dir) do
    {module, orig_ast} = parse(orig_path)
    {^module, patch_ast} = parse(patch_path)

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

    body = do_add_dupa_tracing(orig_body ++ patch_body)

    exports =
      MapSet.union(orig_exports, patch_exports) |> Enum.sort() |> Enum.map(&{:c_var, [], &1})

    specs = orig_specs ++ patch_specs

    specs = Enum.reject(specs, &match?({{:c_literal, _, :spec}, _}, &1))
    ast = {:c_module, [], module_spec, exports, specs, body}

    serialize(ast, out_dir)
  end

  defp parse(path) do
    IO.puts(path)

    {:ok, {_module, [abstract_code: {_backend, abstract_code}]}} =
      :beam_lib.chunks(~c"#{path}", [:abstract_code])

    {:ok, module, ast} = :compile.noenv_forms(abstract_code, [:to_core])

    {module, ast}
  end

  defp serialize(ast, dir) do
    {:ok, module, beam} = :compile.noenv_forms(ast, [:from_core])
    File.write!(Path.join(dir, "#{module}.beam"), beam)
  end

  def add_dupa_tracing(path) do
    {_module, ast} = parse(path)

    ast = do_add_dupa_tracing(ast)

    serialize(ast, Path.dirname(path))
  end

  defp do_add_dupa_tracing(ast) do
    traverse(ast, fn
      {:c_call, call_meta, {:c_literal, mod_meta, mod} = mv, {:c_literal, fun_meta, _fun} = fv,
       args}
      when mod not in [:erlang, :console] ->
        {:c_call, call_meta, {:c_literal, mod_meta, :dupa_trace},
         {:c_literal, fun_meta, :dupa_trace}, [mv, fv | do_add_dupa_tracing(args)]}

      other ->
        {:__cont__, other}
    end)
  end

  defp traverse(ast, fun) do
    with {:__cont__, value} <- fun.(ast) do
      case value do
        tuple when is_tuple(tuple) ->
          tuple |> Tuple.to_list() |> traverse(fun) |> List.to_tuple()

        [h | t] ->
          [traverse(h, fun) | traverse(t, fun)]

        value ->
          value
      end
    end
  end

  defp rename_funs({:c_var, meta, {function, arity} = fa} = entry, ctx)
       when is_atom(function) and is_integer(arity) do
    if fa in ctx.funs do
      entry
    else
      {:c_var, meta, {:"#{ctx.prefix}_#{function}", arity}}
    end
  end

  defp rename_funs({:function, {function, arity} = fa} = entry, ctx)
       when is_atom(function) and is_integer(arity) do
    if fa in ctx.funs do
      entry
    else
      {:function, {:"#{ctx.prefix}_#{function}", arity}}
    end
  end

  defp rename_funs(tuple, ctx) when is_tuple(tuple) do
    tuple |> Tuple.to_list() |> rename_funs(ctx) |> List.to_tuple()
  end

  defp rename_funs([h | t], ctx) do
    [rename_funs(h, ctx) | rename_funs(t, ctx)]
  end

  defp rename_funs(entry, _ctx) do
    entry
  end

  # defp debug(ast) do
  #   File.write!("out.ast.erl", inspect(ast, pretty: true, limit: :infinity))
  #   File.open("out.core", [:write], &:beam_listing.module(&1, ast))
  # end

  # defp replace_calls({:c_apply, apply_meta, {:c_var, var_meta, {fun, arity}}, args}, ctx) do
  #   if {fun, arity} in ctx.funs do
  #     {:c_call, apply_meta, {:c_literal, var_meta, ctx.module}, {:c_literal, var_meta, fun},
  #      replace_calls(args, ctx)}
  #   else
  #     {:c_apply, apply_meta, {:c_var, var_meta, {fun, arity}}, replace_calls(args, ctx)}
  #   end
  # end

  # defp replace_calls(tuple, ctx) when is_tuple(tuple) do
  #   tuple |> Tuple.to_list() |> replace_calls(ctx) |> List.to_tuple()
  # end

  # defp replace_calls([h | t], ctx) do
  #   [replace_calls(h, ctx) | replace_calls(t, ctx)]
  # end

  # defp replace_calls(entry, _ctx) do
  #   entry
  # end
end
