defmodule Treeshake do
  @moduledoc false
  # Tree-shaker for compiled BEAM code. See `run/1` for details.

  @stdlib_apps [:erts, :kernel, :stdlib, :compiler, :elixir, :logger]

  # The modules of these apps are left by default, as tree-shaking can't reason
  # about many of them: they're used through implicit contracts, like calls from
  # the VM or from kernel processes.
  @non_treeshakable_apps [:erts, :stdlib, :kernel, :logger]

  # The modules of the apps above that tree-shaking can reason about, so they're
  # tree-shaken like others. Careful when adding: the calls in left modules are
  # followed only from functions that tree-shaking finds reachable, which misses
  # the ones running in kernel processes, like prim_tty's calls to io_ansi.
  @shakable_stdlib [
    # core libraries
    :erlang,
    :lists,
    :rand,
    :string,
    :unicode_util,
    :uri_string,
    # parsing and printing code
    :beam_lib,
    :epp,
    :erl_parse,
    :erl_pp,
    :erl_scan,
    :ms_transform,
    # shell and tooling
    :argparse,
    :c,
    :edlin_expand,
    :erl_abstract_code,
    :erl_debugger,
    :erl_expand_records,
    :escript,
    :man_docs,
    :otp_internal,
    :seq_trace,
    :shell_default,
    :shell_docs,
    :shell_docs_markdown,
    :trace,
    # networking and distribution
    :auth,
    :erl_ddll,
    :erpc,
    :gen_sctp,
    :global,
    :inet,
    :inet6_sctp,
    :inet6_tcp,
    :inet6_tcp_dist,
    :inet6_udp,
    # :inet_db,
    :inet_dns_tsig,
    :inet_epmd_dist,
    :inet_epmd_socket,
    :inet_gethost_native,
    :inet_hosts,
    :inet_parse,
    :inet_res,
    :inet_sctp,
    :inet_tcp,
    :inet_tcp_dist,
    :kernel_config,
    :local_tcp,
    :local_udp,
    :net_adm,
    :net_kernel,
    :peer,
    :pg2,
    :pool,
    :prim_inet,
    :slave,
    # storage and logging backends
    :dets,
    :dets_server,
    :dets_sup,
    :dets_utils,
    :dets_v9,
    :disk_log,
    :erl_tar,
    :error_logger_file_h,
    :error_logger_tty_h,
    :file_sorter,
    :log_mf_h,
    :logger_disk_log_h,
    :logger_handler,
    :wrap_log_reader,
    :zip,
    :zstd,
    # data structures and others
    :array,
    :dict,
    :digraph,
    :digraph_utils,
    :erl_reply,
    :gen_fsm,
    :graph,
    :qlc,
    :qlc_pt,
    :random,
    :sofs
  ]

  @default_ignore [mod: :prim_eval]

  @hardcoded %{
    calls: %{
      Supervisor => [{Supervisor.Default, :init, 1}],
      {:elixir_config, :identifier_tokenizer, 0} => [{String.Tokenizer, :tokenize, 1}],
      # The translators from the :logger app's environment
      Logger.Utils => [{Logger.Translator, :translate, 4}],
      # The formatter of the default handler, called by :logger
      Logger.App => [{Logger.Formatter, :format, 2}],
      # The backend stored in a persistent term (telemetry 1.4)
      :telemetry_handler_table => [:telemetry_ets, :telemetry_pt],
      # When it's JSON
      {Phoenix, :json_library, 0} => [JSON],
      # `form[field]`: Phoenix.HTML.Form implements Access without declaring
      # the behaviour
      Access => [Phoenix.HTML.Form],
      # Through the form's Phoenix.HTML.FormData implementation, bypassing
      # the protocol
      Phoenix.HTML.Form => [Phoenix.HTML.FormData]
    },
    behaviour_impls: %{
      :application_controller => [:gen_server]
    }
  }

  @type keep_entry ::
          {:mod, module()}
          | {:mfa, mfa()}
          | {:behaviour_impls, module()}
          | {:referenced_modules_defining, {atom(), non_neg_integer()}}

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
          | {:drop, [{:mod, module()}]}
          | {:leave, [{:mod, module()}]}
          | {:ignore, [{:mod, module()} | {:mfa, mfa()}]}

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
  - callbacks of applications' modules (`start/2`, `stop/1`, etc.)
  - functions called or captured (`&Module.fun/arity`) by a reachable function
  - functions referenced as a hard-coded module-function-arity (MFA) tuple
    or module-function-arguments tuple in a reachable function
  - callback implementations of behaviours, if their module is referenced
    in a reachable function
  - macro-generated functions in modules implementing behaviours, if the module
    is referenced in a reachable function
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
  all take keyword lists and default to an empty list.
  - `keep` - What must not be removed, along with all code it relies on:
    `mod: module` means 'all public functions of this module',
    `mfa: {module, fun, arity}` means 'this public function',
    `behaviour_impls: behaviour` means 'all modules implementing this behaviour',
    and `referenced_modules_defining: {fun, arity}` means 'all modules defining this
    public function, once a reachable function references them'.
    Applications' callbacks are automatically added to this list.
  - `drop` - Modules to be removed regardless of the tree-shaking results, as
    `mod: module` entries.
  - `leave` - Modules that must not be changed or removed, as `mod: module` entries,
    but without affecting the tree-shaking process. It means that the code they rely on may
    still be removed unless added to `keep` or `leave`.
  - `ignore` - Modules or functions that are not analyzed by the tree-shaker, as
    `mod: module` and `mfa: {module, fun, arity}` entries.
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
  #{list_atoms.(@shakable_stdlib)}.

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
    add_stubs = opts.stub_removed_functions or opts.stub_removed_modules

    if not opts.dry_run and add_stubs do
      helper_path = :code.which(:treeshake_helper)
      File.cp!(helper_path, Path.join(opts.output_dir, Path.basename(helper_path)))
    end

    stats
  end

  defp parse_opts(opts) do
    if Keyword.take(opts, [:ebin_files, :project]) == [] do
      raise "Either :ebin_files or :project option must be provided"
    end

    validate_entries!(opts, :keep, [:mod, :mfa, :behaviour_impls, :referenced_modules_defining])
    validate_entries!(opts, :drop, [:mod])
    validate_entries!(opts, :leave, [:mod])
    validate_entries!(opts, :ignore, [:mod, :mfa])

    opts =
      opts
      |> Keyword.put_new(:verbose, false)
      |> Keyword.put_new(:dry_run, false)
      |> Keyword.put_new(:stub_removed_functions, false)
      |> Keyword.put_new(:stub_removed_modules, false)
      |> Keyword.put_new(:drop, [])
      |> keyword_concat_default(:ignore, @default_ignore)

    dropped = for {:mod, module} <- opts[:drop], do: module
    default_leave = for module <- non_treeshakable_stdlib_modules() -- dropped, do: {:mod, module}
    opts = keyword_concat_default(opts, :leave, default_leave)

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
        # The application callbacks, optional ones included
        [
          mfa: {mod, :start, 2},
          mfa: {mod, :stop, 1},
          mfa: {mod, :prep_stop, 1},
          mfa: {mod, :config_change, 3}
        ]
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
    |> then(&(&1 -- @shakable_stdlib))
  end

  defp beam_module(beam_path) do
    beam_path |> Path.basename(".beam") |> String.to_atom()
  end

  defp filter_ext(paths, ext) do
    Enum.filter(paths, fn path -> Path.extname(path) == ext end)
  end

  @entry_kinds %{
    mod: "mod: module",
    mfa: "mfa: {module, fun, arity}",
    behaviour_impls: "behaviour_impls: behaviour",
    referenced_modules_defining: "referenced_modules_defining: {fun, arity}"
  }

  defp validate_entries!(opts, option, kinds) do
    for entry <- Keyword.get(opts, option, []), not valid_entry?(entry, kinds) do
      expected = Enum.map_join(kinds, ", ", &Map.fetch!(@entry_kinds, &1))

      raise ArgumentError,
            "invalid #{inspect(option)} entry #{inspect(entry)}, expected #{expected}"
    end
  end

  defp valid_entry?({:mod, module}, kinds), do: :mod in kinds and is_atom(module)

  defp valid_entry?({:mfa, {module, fun, arity}}, kinds),
    do: :mfa in kinds and is_atom(module) and is_atom(fun) and is_integer(arity) and arity >= 0

  defp valid_entry?({:behaviour_impls, behaviour}, kinds),
    do: :behaviour_impls in kinds and is_atom(behaviour)

  defp valid_entry?({:referenced_modules_defining, {fun, arity}}, kinds),
    do:
      :referenced_modules_defining in kinds and is_atom(fun) and is_integer(arity) and arity >= 0

  defp valid_entry?(_entry, _kinds), do: false

  defp keyword_concat_default(kw, key, default) do
    Keyword.update(kw, key, default, &(&1 ++ default))
  end
end
