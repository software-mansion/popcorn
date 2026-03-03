defmodule Phoenix.Template do
  @moduledoc """
  Templates are markup languages that are compiled to Elixir code.

  This module provides functions for loading and compiling templates
  from disk. A markup language is compiled to Elixir code via an engine.
  See `Phoenix.Template.Engine`.

  In practice, developers rarely use `Phoenix.Template` directly. Instead,
  libraries such as `Phoenix.View` and `Phoenix.LiveView` use it as a
  building block.

  ## Custom Template Engines

  Phoenix supports custom template engines. Engines tell
  Phoenix how to convert a template path into quoted expressions.
  See `Phoenix.Template.Engine` for more information on
  the API required to be implemented by custom engines.

  Once a template engine is defined, you can tell Phoenix
  about it via the template engines option:

      config :phoenix_template, :template_engines,
        eex: Phoenix.Template.EExEngine,
        exs: Phoenix.Template.ExsEngine

  ## Format encoders

  Besides template engines, Phoenix has the concept of format encoders.
  Format encoders work per format and are responsible for encoding a
  given format to a string. For example, when rendering JSON, your
  templates may return a regular Elixir map. Then the JSON format
  encoder is invoked to convert it to JSON.

  A format encoder must export a function called `encode_to_iodata!/1`
  which receives the rendering artifact and returns iodata.

  New encoders can be added via the format encoder option:

      config :phoenix_template, :format_encoders,
        html: Phoenix.HTML.Engine

  """

  @type path :: binary
  @type root :: binary

  @default_pattern "*"

  @doc """
  Ensure `__mix_recompile__?/0` will be defined.
  """
  defmacro __using__(_opts) do
    quote do
      Phoenix.Template.__idempotent_setup__(__MODULE__, %{})
    end
  end

  @doc """
  A convenience macro for embeding templates as functions.

  This macro is built on top of the more general `compile_all/3`
  functionality.

  ## Options

    * `:root` - The root directory to embed files. Defaults to the current
      module's directory (`__DIR__`)
    * `:suffix` - The string value to append to embedded function names. By
      default, function names will be the name of the template file excluding
      the format and engine.

  A wildcard pattern may be used to select all files within a directory tree.
  For example, imagine a directory listing:

      ├── pages
      │   ├── about.html.heex
      │   └── sitemap.xml.eex

  Then to embed the templates in your module:

      defmodule MyAppWeb.Renderer do
        import Phoenix.Template, only: [embed_templates: 1]
        embed_templates "pages/*"
      end

  Now, your module will have a `about/1` and `sitemap/1` functions.
  Note that functions across different formats were embedded. In case
  you want to distinguish between them, you can give a more specific
  pattern:

      defmodule MyAppWeb.Emails do
        import Phoenix.Template, only: [embed_templates: 2]

        embed_templates "pages/*.html", suffix: "_html"
        embed_templates "pages/*.xml", suffix: "_xml"
      end

  Now the functions will be `about_html` and `sitemap_xml`.
  """
  @doc type: :macro
  defmacro embed_templates(pattern, opts \\ []) do
    quote bind_quoted: [pattern: pattern, opts: opts] do
      Phoenix.Template.compile_all(
        &Phoenix.Template.__embed__(&1, opts[:suffix]),
        Path.expand(opts[:root] || __DIR__, __DIR__),
        pattern
      )
    end
  end

  @doc false
  def __embed__(path, suffix),
    do:
      path
      |> Path.basename()
      |> Path.rootname()
      |> Path.rootname()
      |> Kernel.<>(suffix || "")

  @doc """
  Renders the template and returns iodata.
  """
  def render_to_iodata(module, template, format, assign) do
    module
    |> render(template, format, assign)
    |> encode(format)
  end

  @doc """
  Renders the template to string.
  """
  def render_to_string(module, template, format, assign) do
    module
    |> render_to_iodata(template, format, assign)
    |> IO.iodata_to_binary()
  end

  @doc """
  Renders template from module.

  For a module called `MyApp.FooHTML` and template "index.html.heex",
  it will:

    * First attempt to call `MyApp.FooHTML.index(assigns)`

    * Then fallback to `MyApp.FooHTML.render("index.html", assigns)`

    * Raise otherwise

  It expects the HTML module, the template as a string, the format, and a
  set of assigns.

  Notice that this function returns the inner representation of a
  template. If you want the encoded template as a result, use
  `render_to_iodata/4` instead.

  ## Examples

      Phoenix.Template.render(YourApp.UserView, "index", "html", name: "John Doe")
      #=> {:safe, "Hello John Doe"}

  ## Assigns

  Assigns are meant to be user data that will be available in templates.
  However, there are keys under assigns that are specially handled by
  Phoenix, they are:

    * `:layout` - tells Phoenix to wrap the rendered result in the
      given layout. See next section

  ## Layouts

  Templates can be rendered within other templates using the `:layout`
  option. `:layout` accepts a tuple of the form
  `{LayoutModule, "template.extension"}`.

  To template that goes inside the layout will be placed in the `@inner_content`
  assign:

      <%= @inner_content %>

  """
  def render(module, template, format, assigns) do
    assigns
    |> Map.new()
    |> Map.pop(:layout, false)
    |> render_within_layout(module, template, format)
  end

  defp render_within_layout({false, assigns}, module, template, format) do
    render_with_fallback(module, template, format, assigns)
  end

  defp render_within_layout({{layout_mod, layout_tpl}, assigns}, module, template, format)
       when is_atom(layout_mod) and is_binary(layout_tpl) do
    content = render_with_fallback(module, template, format, assigns)
    assigns = Map.put(assigns, :inner_content, content)
    render_with_fallback(layout_mod, layout_tpl, format, assigns)
  end

  defp render_within_layout({layout, _assigns}, _module, _template, _format) do
    raise ArgumentError, """
    invalid value for reserved key :layout in Phoenix.Template.render/4 assigns.
    :layout accepts a tuple of the form {LayoutModule, "template.extension"},
    got: #{inspect(layout)}
    """
  end

  defp encode(content, format) do
    if encoder = format_encoder(format) do
      encoder.encode_to_iodata!(content)
    else
      content
    end
  end

  defp render_with_fallback(module, template, format, assigns)
       when is_atom(module) and is_binary(template) and is_binary(format) and is_map(assigns) do
    :erlang.module_loaded(module) or :code.ensure_loaded(module)

    try do
      String.to_existing_atom(template)
    catch
      _, _ -> fallback_render(module, template, format, assigns)
    else
      atom ->
        if function_exported?(module, atom, 1) do
          apply(module, atom, [assigns])
        else
          fallback_render(module, template, format, assigns)
        end
    end
  end

  @compile {:inline, fallback_render: 4}
  defp fallback_render(module, template, format, assigns) do
    if function_exported?(module, :render, 2) do
      module.render(template <> "." <> format, assigns)
    else
      reason =
        if Code.ensure_loaded?(module) do
          " (the module exists but does not define #{template}/1 nor render/2)"
        else
          " (the module does not exist)"
        end

      raise ArgumentError,
            "no \"#{template}\" #{format} template defined for #{inspect(module)} #{reason}"
    end
  end

  ## Configuration API

  @doc """
  Returns the format encoder for the given template.
  """
  @spec format_encoder(format :: String.t()) :: module | nil
  def format_encoder(format) when is_binary(format) do
    Map.get(compiled_format_encoders(), format)
  end

  defp compiled_format_encoders do
    case Application.fetch_env(:phoenix_template, :compiled_format_encoders) do
      {:ok, encoders} ->
        encoders

      :error ->
        encoders =
          default_encoders()
          |> Keyword.merge(raw_config(:format_encoders, []))
          |> Enum.filter(fn {_, v} -> v end)
          |> Enum.into(%{}, fn {k, v} -> {to_string(k), v} end)

        Application.put_env(:phoenix_template, :compiled_format_encoders, encoders)
        encoders
    end
  end

  defp default_encoders do
    [html: Phoenix.HTML.Engine, json: json_library(), js: Phoenix.HTML.Engine]
  end

  defp json_library() do
    Application.get_env(:phoenix_template, :json_library) ||
      deprecated_config(:phoenix_view, :json_library) ||
      Application.get_env(:phoenix, :json_library, Jason)
  end

  @doc """
  Returns a keyword list with all template engines
  extensions followed by their modules.
  """
  @spec engines() :: %{atom => module}
  def engines do
    compiled_engines()
  end

  defp compiled_engines do
    case Application.fetch_env(:phoenix_template, :compiled_template_engines) do
      {:ok, engines} ->
        engines

      :error ->
        engines =
          default_engines()
          |> Keyword.merge(raw_config(:template_engines, []))
          |> Enum.filter(fn {_, v} -> v end)
          |> Enum.into(%{})

        Application.put_env(:phoenix_template, :compiled_template_engines, engines)
        engines
    end
  end

  defp default_engines do
    [
      eex: Phoenix.Template.EExEngine,
      exs: Phoenix.Template.ExsEngine,
      leex: Phoenix.LiveView.Engine,
      heex: Phoenix.LiveView.HTMLEngine
    ]
  end

  defp raw_config(name, fallback) do
    Application.get_env(:phoenix_template, name) ||
      deprecated_config(:phoenix_view, name) ||
      Application.get_env(:phoenix, name, fallback)
  end

  defp deprecated_config(app, name) do
    if value = Application.get_env(app, name) do
      IO.warn(
        "config :#{app}, :#{name} is deprecated, please use config :phoenix_template, :#{name} instead"
      )

      value
    end
  end

  ## Lookup API

  @doc """
  Returns all template paths in a given template root.
  """
  @spec find_all(root, pattern :: String.t(), %{atom => module}) :: [path]
  def find_all(root, pattern \\ @default_pattern, engines \\ engines()) do
    extensions = engines |> Map.keys() |> Enum.join(",")

    root
    |> Path.join(pattern <> ".{#{extensions}}")
    |> Path.wildcard()
  end

  @doc """
  Returns the hash of all template paths in the given root.

  Used by Phoenix to check if a given root path requires recompilation.
  """
  @spec hash(root, pattern :: String.t(), %{atom => module}) :: binary
  def hash(root, pattern \\ @default_pattern, engines \\ engines()) do
    find_all(root, pattern, engines)
    |> Enum.sort()
    |> :erlang.md5()
  end

  @doc """
  Compiles a function for each template in the given `root`.

  `converter` is an anonymous function that receives the template path
  and returns the function name (as a string).

  For example, to compile all `.eex` templates in a given directory,
  you might do:

      Phoenix.Template.compile_all(
        &(&1 |> Path.basename() |> Path.rootname(".eex")),
        __DIR__,
        "*.eex"
      )

  If the directory has templates named `foo.eex` and `bar.eex`,
  they will be compiled into the functions `foo/1` and `bar/1`
  that receive the template `assigns` as argument.

  You may optionally pass a keyword list of engines. If a list
  is given, we will lookup and compile only this subset of engines.
  If none is passed (`nil`), the default list returned by `engines/0`
  is used.
  """
  defmacro compile_all(converter, root, pattern \\ @default_pattern, engines \\ nil) do
    quote bind_quoted: binding() do
      for {path, name, body} <-
            Phoenix.Template.__compile_all__(__MODULE__, converter, root, pattern, engines) do
        @external_resource path
        @file path
        def unquote(String.to_atom(name))(var!(assigns)) do
          _ = var!(assigns)
          unquote(body)
        end

        {name, path}
      end
    end
  end

  @doc false
  def __compile_all__(module, converter, root, pattern, given_engines) do
    engines = given_engines || engines()
    paths = find_all(root, pattern, engines)

    {triplets, {paths, engines}} =
      Enum.map_reduce(paths, {[], %{}}, fn path, {acc_paths, acc_engines} ->
        ext = Path.extname(path) |> String.trim_leading(".") |> String.to_atom()
        engine = Map.fetch!(engines, ext)
        name = converter.(path)
        body = engine.compile(path, name)
        map = {path, name, body}
        reduce = {[path | acc_paths], Map.put(acc_engines, engine, true)}
        {map, reduce}
      end)

    # Store the engines so we define compile-time deps
    __idempotent_setup__(module, engines)

    # Store the hashes so we define __mix_recompile__?
    hash = paths |> Enum.sort() |> :erlang.md5()

    args =
      if given_engines, do: [root, pattern, Macro.escape(given_engines)], else: [root, pattern]

    Module.put_attribute(module, :phoenix_templates_hashes, {hash, args})
    triplets
  end

  @doc false
  def __idempotent_setup__(module, engines) do
    # Store the used engines so they become requires on before_compile
    if used_engines = Module.get_attribute(module, :phoenix_templates_engines) do
      Module.put_attribute(module, :phoenix_templates_engines, Map.merge(used_engines, engines))
    else
      Module.register_attribute(module, :phoenix_templates_hashes, accumulate: true)
      Module.put_attribute(module, :phoenix_templates_engines, engines)
      Module.put_attribute(module, :before_compile, Phoenix.Template)
    end
  end

  @doc false
  defmacro __before_compile__(env) do
    hashes = Module.get_attribute(env.module, :phoenix_templates_hashes)
    engines = Module.get_attribute(env.module, :phoenix_templates_engines)

    body =
      Enum.reduce(hashes, false, fn {hash, args}, acc ->
        quote do
          unquote(acc) or unquote(hash) != Phoenix.Template.hash(unquote_splicing(args))
        end
      end)

    compile_time_deps =
      for {engine, _} <- engines do
        quote do
          unquote(engine).__info__(:module)
        end
      end

    quote do
      unquote(compile_time_deps)

      @doc false
      def __mix_recompile__? do
        unquote(body)
      end
    end
  end

  ## Deprecated API

  @deprecated "Use Phoenix.View.template_path_to_name/3"
  def template_path_to_name(path, root) do
    path
    |> Path.rootname()
    |> Path.relative_to(root)
  end

  @deprecated "Use Phoenix.View.module_to_template_root/3"
  def module_to_template_root(module, base, suffix) do
    module
    |> unsuffix(suffix)
    |> Module.split()
    |> Enum.drop(length(Module.split(base)))
    |> Enum.map(&Macro.underscore/1)
    |> join_paths()
  end

  defp join_paths([]), do: ""
  defp join_paths(paths), do: Path.join(paths)

  defp unsuffix(value, suffix) do
    string = to_string(value)
    suffix_size = byte_size(suffix)
    prefix_size = byte_size(string) - suffix_size

    case string do
      <<prefix::binary-size(prefix_size), ^suffix::binary>> -> prefix
      _ -> string
    end
  end
end
