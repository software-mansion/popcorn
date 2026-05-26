defmodule Treeshake do
  @moduledoc """
  Tree-shaker for compiled BEAM code. See `run/1` for details.
  """

  @stdlib_apps [:erts, :kernel, :stdlib, :compiler, :elixir, :logger]
  @non_treeshakable_apps [:erts, :stdlib, :kernel, :logger]

  @non_treeshakable_exclusions [
    :unicode_util,
    :erl_parse,
    :epp,
    :erl_scan,
    :prim_inet,
    :qlc,
    :qlc_pt,
    :dets_v9,
    :dets,
    :sofs,
    :erl_tar,
    :file_sorter,
    :global,
    :disk_log,
    :net_kernel,
    :zip,
    # :inet_db,
    :edlin_expand,
    :dets_utils,
    :erl_pp,
    :beam_lib,
    :ms_transform,
    :lists,
    :erlang,
    :string,
    :uri_string,
    :inet,
    :rand
  ]

  @default_ignore_modules [:prim_eval]

  @hardcoded %{
    calls: %{
      Supervisor => [{Supervisor.Default, :init, 1}]
    },
    behaviour_impls: %{
      :application_controller => [:gen_server]
    }
  }

  @type keep_entry :: mfa() | module() | %{behaviour_impls: module()}

  @type run_option ::
          {:ebin_files, [Path.t()]}
          | {:project, Path.t()}
          | {:output_dir, Path.t()}
          | {:dry_run, boolean()}
          | {:include_stdlibs, boolean()}
          | {:stub_removed_functions, boolean()}
          | {:stub_removed_modules, boolean()}
          | {:verbose, boolean()}
          | {:keep, [keep_entry()]}
          | {:drop, [module()]}
          | {:leave, [module()]}
          | {:ignore, [module() | mfa()]}

  @type stats :: %{
          modules_removed: [module()],
          modules_shaked: %{module() => [{function :: atom(), arity :: non_neg_integer()}]},
          output_dir: Path.t(),
          module_index: %{module() => module_info :: map()},
          call_graph: %{mfa() => [mfa()]}
        }

  @typep config :: %{
           opts: map(),
           module_index: nil | %{module() => map()},
           call_graph: nil | %{mfa() => [mfa()]}
         }

  list_atoms = fn atoms -> atoms |> Enum.sort() |> Enum.map_join(", ", &"`#{&1}`") end

  @doc """
  Removes unused modules and functions from compiled BEAM code.

  The removal is done by tree-shaking on the function level: reachable functions
  are found and all others are removed. The following functions are considered
  reachable:
  - `start/2` functions of applications
  - functions called or captured (`&Module.fun/arity`) by a reachable function
  - functions referenced as a hard-coded module-function-arity (MFA) tuple
    or module-function-arguments tuple in a reachable function
  - callback implementations of behaviours, if their module is referenced
    in a reachable function
  - callback implementations of protocols, if their module is referenced
    and the protocol function is called in a reachable function

  These cases cover vast majority, but not all used functions. For example, creating
  a module name dynamically from a string and calling a function from it may not make
  the function reachable, even though it's used. On the other hand, some unused functions
  may be considered reachable, for example when they're called by a reachable function,
  but from a branch of code that is never executed. Therefore, it's possible
  to manually inform the tree-shaker about reachability of particular modules
  and functions - see 'Manual adjustment of tree-shaking'.

  Additionally, some modules are excluded from tree-shaking (left as is), but it
  doesn't impact reachability of their functions. See 'Modules left by default'
  for more details.

  ## Accepted options

  - `ebin_files` - List of `.beam` and `.app` files to be tree-shaked. The beam files
    must have `abstract code` chunk in them (included by default by the compiler).
    This option is mandatory, unless `project` option is provided.
  - `project` - Path to the root directory of a project to be tree-shaked. The project
    must be compiled with `MIX_ENV=prod`. If `ebin_files` are also passed, they
    are included as well.
  - `output_dir` - Path to the directory where the tree-shaked ebin files are stored.
    Mandatory, unless `dry_run` is set to `true`.
  - `dry_run` - If `true`, no output is written.
  - `include_stdlibs` - If `true`, Erlang and Elixir standard libraries are included
    and tree-shaked, making the output self-contained. The following applications
    are included: #{list_atoms.(@stdlib_apps)}. Defaults to `false`.
  - `stub_removed_functions` - If `true`, replace removed functions with stubs
    that raise an error. Defaults to `false`.
  - `stub_removed_modules` - If `true`, replace removed modules with empty stubs.
    Has no effect when `stub_removed_functions` is set to `true`. Defaults to `false`.
  - `verbose` - Print status logs.

  ### Manual adjustment of tree-shaking

  Additionally, the following options configure the tree-shaking behavior. They
  all default to an empty list.
  - `keep` - List of public functions that must not be removed along with all code
    they rely on. Passing a module means 'all functions from this module', passing
    `%{benaviour_impls: behaviour}` means 'all modules implementing this behaviour'.
    Applications' `start/2` functions are automatically added to this list.
  - `drop` - List of modules to be removed regardless of the tree-shaking results
  - `leave` - List of modules that must not be changed or removed, but without
    affecting the tree-shaking process. It means that the code they rely on may
    still be removed unless added to `keep` or `leave`. 
  - `ignore` - List of modules or functions that are not analyzed by the tree-shaker.
    It means they can be tree-shaked as usual, but the code they rely on may be
    tree-shaked even if they're kept, as it is unknown to the tree-shaker.
    This option accepts also private functions. Note that if an ignored function
    calls a private function and that function gets tree-shaked, the result won't
    compile and tree-shaking will fail.
    When a module is in both `leave` and `ignore` lists, it's not read and it's copied
    to the output without changes.

  ## Modules left by default

  Some stdlib modules are hard to tree-shake and they're therefore left by default.
  The behaviour is the same as if they were in the `leave` list. These modules can still
  be removed if added to `drop` or `ignore` list.

  The modules from the following stdlib apps: #{list_atoms.(@non_treeshakable_apps)},
  are left by default, except the following modules:
  #{list_atoms.(@non_treeshakable_exclusions)}.

  ## Returned value
  This function returns a map with the following keys:
  - `modules_removed` - list of modules completely removed due to tree-shaking
  - `modules_shaked` - a map where each key is a module and value is a list of
    the functions removed from this module
  - `output_dir` - the directory where output ebin files were written
  - `module_index` - information about the tree-shaked modules, for debugging
    purposes
  - `call_graph` - graph of calls within tree-shaked modules, for debugging
    purposes

  Note that when `stub_removed_functions` or `stub_removed_modules` is set to `true`,
  `modules_removed` and `modules_shaked` contain stubbed modules and functions.
  """
  @spec run([run_option()]) :: stats()
  def run(opts) do
    opts
    |> config()
    |> build_module_index()
    |> build_call_graph()
    |> shake()
  end

  @doc false
  @spec config([run_option()]) :: config()
  def config(opts) do
    opts = parse_opts(opts)
    %{opts: opts, module_index: nil, call_graph: nil}
  end

  @doc false
  @spec build_module_index(config()) :: config()
  def build_module_index(%{opts: opts} = config) do
    if opts.verbose, do: IO.puts("Building module index")

    module_index = Treeshake.ModuleIndex.build(opts, @hardcoded)

    %{config | module_index: module_index}
  end

  @doc false
  @spec build_call_graph(config()) :: config()
  def build_call_graph(%{opts: opts, module_index: module_index} = config)
      when module_index != nil do
    if opts.verbose, do: IO.puts("Creating call graph")
    %{config | call_graph: Treeshake.CallGraph.create(module_index, opts.keep)}
  end

  @doc false
  @spec shake(config()) :: stats()
  def shake(%{opts: opts, module_index: module_index, call_graph: call_graph})
      when module_index != nil and call_graph != nil do
    if opts.verbose, do: IO.puts("Shaking")

    unless Map.has_key?(opts, :output_dir) or opts.dry_run do
      raise "Missing required option: output_dir"
    end

    stats = Treeshake.Shaker.shake(opts, call_graph, module_index)

    unless opts.dry_run do
      helper_path = :code.which(:treeshake_helper)
      File.cp!(helper_path, Path.join(opts.output_dir, Path.basename(helper_path)))
    end

    stats
  end

  defp parse_opts(opts) do
    if Keyword.take(opts, [:ebin_files, :project]) == [] do
      raise "Either :ebin_files or :project option must be provided"
    end

    opts =
      opts
      |> Keyword.put_new(:verbose, false)
      |> Keyword.put_new(:dry_run, false)
      |> Keyword.put_new(:drop, [])
      |> keyword_concat_default(:ignore, @default_ignore_modules)

    default_leave = non_treeshakable_stdlib_modules()
    opts = keyword_concat_default(opts, :leave, default_leave -- opts[:drop])

    {project, opts} = Keyword.pop(opts, :project)

    project_ebin_files =
      case project do
        nil ->
          []

        project_path ->
          mix_env = "prod"
          path = Path.expand(project_path)
          build_dir = Path.join([path, "_build", mix_env])

          if not File.dir?(build_dir) do
            raise "Run `MIX_ENV=#{mix_env} mix compile` first. Expected: #{build_dir}"
          end

          find_ebin_files(build_dir)
      end

    ebin_files =
      Keyword.get(opts, :ebin_files, []) ++
        project_ebin_files ++
        if Keyword.get(opts, :include_stdlibs, false), do: get_stdlibs(), else: []

    ebin_files = Enum.uniq_by(ebin_files, &Path.basename/1)
    app_files = filter_ext(ebin_files, ".app")

    opts =
      opts
      |> Keyword.put(:ebin_files, ebin_files)
      |> keyword_concat_default(:keep, detect_entry_points(app_files))

    if opts[:keep] == [] do
      raise "No entry points found"
    end

    Map.new(opts)
  end

  defp detect_entry_points(app_files) do
    Enum.flat_map(app_files, fn app_file ->
      with {:ok, [{:application, _name, attrs}]} <-
             :file.consult(String.to_charlist(app_file)),
           {mod, _args} <- Keyword.get(attrs, :mod) do
        [{mod, :start, 2}]
      else
        _not_found -> []
      end
    end)
  end

  defp find_ebin_files(build_dir) do
    consolidated = build_dir |> Path.join("**/consolidated/*") |> Path.wildcard()
    ebin = build_dir |> Path.join("**/ebin/*") |> Path.wildcard()
    Enum.uniq_by(consolidated ++ ebin, &Path.basename/1)
  end

  defp get_stdlibs() do
    Enum.flat_map(@stdlib_apps, fn app ->
      app |> :code.lib_dir() |> Path.join("ebin/*") |> Path.wildcard()
    end)
  end

  defp non_treeshakable_stdlib_modules() do
    @non_treeshakable_apps
    |> Enum.flat_map(fn app ->
      app |> :code.lib_dir() |> Path.join("ebin/*.beam") |> Path.wildcard()
    end)
    |> Enum.map(&beam_module/1)
    |> then(&(&1 -- @non_treeshakable_exclusions))
  end

  defp beam_module(beam_path) do
    beam_path |> Path.basename(".beam") |> String.to_atom()
  end

  defp filter_ext(paths, ext) do
    Enum.filter(paths, fn path -> Path.extname(path) == ext end)
  end

  defp keyword_concat_default(kw, key, default) do
    Keyword.update(kw, key, default, &(&1 ++ default))
  end
end
