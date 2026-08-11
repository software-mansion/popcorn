defmodule Popcorn.BeamTools.BeamPatcher do
  @moduledoc """
  Replaces functions in a compiled BEAM with implementations taken from a patch
  module, by merging both modules as Core Erlang and recompiling with the host
  compiler.

  A patch function replaces the original of the same name and arity, including
  for calls made from the original module's own code. Exported patch functions
  listed in `-compile([{popcorn_patch_private, [{f, a}]}])` stay private.
  `popcorn_module:f(...)` calls the original implementation of `f`.
  """

  @orig_prefix "popcorn_orig"
  @patch_prefix "popcorn_patch"

  @doc """
  Compiles `patch_path` and merges it into the BEAM at `beam_path`, overwriting it.
  """
  def patch_beam(beam_path, patch_path) do
    with {:ok, original} <- read_ast(beam_path),
         {:ok, patch} <- compile_patch(patch_path) do
      beam =
        original
        |> merge_modules(patch)
        |> serialize()

      File.write!(beam_path, beam)
      :ok
    end
  end

  defp read_ast(beam_path) do
    case :beam_lib.chunks(to_charlist(beam_path), [:abstract_code]) do
      {:ok, {_module, [abstract_code: {_backend, abstract_code}]}} ->
        {:ok, _module, ast} = :compile.noenv_forms(abstract_code, [:to_core])
        {:ok, ast}

      _ ->
        err(:unpatchable_beam, beam_path)
    end
  end

  defp compile_patch(patch_path) do
    case :compile.file(to_charlist(patch_path), [:binary, :debug_info, :return_errors, :to_core]) do
      {:ok, _module, ast} ->
        {:ok, ast}

      {:error, errors, _warnings} ->
        err(:bad_patch, {patch_path, errors})
    end
  end

  defp serialize(ast) do
    {:ok, _module, beam} = :compile.noenv_forms(ast, [:from_core, debug_info: {:core_v1, ast}])
    beam
  end

  defp merge_modules(orig_ast, patch_ast) do
    {:c_module, _meta, module_spec, orig_exports, orig_specs, orig_body} = orig_ast
    {:c_module, _meta, _module_spec, patch_exports, patch_specs, patch_body} = patch_ast
    private_overrides = private_overrides(patch_specs)
    orig_exports = MapSet.new(orig_exports, fn {:c_var, _meta, fa} -> fa end)

    patch_exports =
      patch_exports
      |> MapSet.new(fn {:c_var, _meta, fa} -> fa end)
      |> MapSet.difference(private_overrides)
      |> MapSet.difference(MapSet.new(module_info: 0, module_info: 1))

    exports = MapSet.union(orig_exports, patch_exports)
    replaced = MapSet.union(patch_exports, private_overrides)

    # Everything the patch does not replace keeps its name: the module's own
    # `-nifs` declarations and internal calls refer to private functions by name.
    # Replaced originals are renamed rather than dropped so a patch can delegate
    # to them; an original nothing delegates to is dead code and the compiler
    # removes it. Patch-private helpers are renamed so they cannot collide with
    # an original of the same name.
    orig_body = rename_replaced_funs(orig_body, replaced)

    patch_body =
      patch_body
      |> rename_funs_and_local_calls(%{prefix: @patch_prefix, except: replaced})
      |> inject_original_calls(replaced)

    exports_ast = exports |> Enum.sort() |> Enum.map(&{:c_var, [], &1})

    {:c_module, [], module_spec, exports_ast, orig_specs ++ patch_specs, patch_body ++ orig_body}
  end

  defp private_overrides(patch_specs) do
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
  end

  defp rename_replaced_funs(ast, funs) do
    Enum.map(ast, fn
      {{:c_var, var_meta, {fun, arity}}, definition} ->
        if {fun, arity} in funs do
          {{:c_var, var_meta, {prefixed(@orig_prefix, fun), arity}}, definition}
        else
          {{:c_var, var_meta, {fun, arity}}, definition}
        end

      form ->
        form
    end)
  end

  defp inject_original_calls(ast, replaced) do
    with {:c_call, call_meta, mod_ast, fun_ast, args} <- ast,
         {:c_literal, _mod_meta, :popcorn_module} <- mod_ast,
         {:c_literal, fun_meta, fun} <- fun_ast do
      arity = length(args)
      original = if {fun, arity} in replaced, do: prefixed(@orig_prefix, fun), else: fun

      {:c_apply, call_meta, {:c_var, fun_meta, {original, arity}}, args}
    else
      _other -> traverse(ast, &inject_original_calls(&1, replaced))
    end
  end

  defp rename_funs_and_local_calls({:c_var, meta, {function, arity} = fa} = ast, ctx)
       when is_atom(function) and is_integer(arity) do
    if fa in ctx.except do
      ast
    else
      {:c_var, meta, {prefixed(ctx.prefix, function), arity}}
    end
  end

  defp rename_funs_and_local_calls({:function, {function, arity} = fa} = ast, ctx)
       when is_atom(function) and is_integer(arity) do
    if fa in ctx.except do
      ast
    else
      {:function, {prefixed(ctx.prefix, function), arity}}
    end
  end

  defp rename_funs_and_local_calls({:id, {line, col, id}}, ctx) do
    id =
      case Atom.to_string(id) do
        "-" <> id -> id
        id -> id
      end

    {:id, {line, col, prefixed("-" <> ctx.prefix, id)}}
  end

  defp rename_funs_and_local_calls(ast, ctx) do
    traverse(ast, &rename_funs_and_local_calls(&1, ctx))
  end

  defp prefixed(prefix, name), do: String.to_atom("#{prefix}_#{name}")

  defp traverse(ast, fun) when is_tuple(ast) do
    ast |> Tuple.to_list() |> fun.() |> List.to_tuple()
  end

  defp traverse([h | t], fun) do
    [fun.(h) | fun.(t)]
  end

  defp traverse(ast, _fun) do
    ast
  end

  defp err(:unpatchable_beam, beam_path) do
    {:error, %{code: "unpatchable_beam", beam: beam_path}}
  end

  defp err(:bad_patch, {patch_path, errors}) do
    {:error, %{code: "bad_patch", patch: patch_path, errors: inspect(errors)}}
  end
end
