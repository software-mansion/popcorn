defmodule Mix.Tasks.Llv.InstallTest do
  use ExUnit.Case, async: true

  import Igniter.Test

  # Note about parens: `socket(...)`/`plug(...)` show up with parens in test
  # diffs because the virtual test project doesn't pick up Phoenix's
  # locals_without_parens formatter rules (deps not physically compiled).
  # In a real Phoenix project after `mix llv.install`, mix format strips them.
  #
  # Patch DSL: each line is `<metadata>|<content>`. `+` = added, `-` = removed,
  # blank = context. Substring match on the diff.
  # See deps/igniter/lib/igniter/test.ex (Igniter.Test.sanitize_diff/2).
  # To debug a new assertion, pipe through `puts_diff(only: "<path>")` first.

  @endpoint_path "lib/test_app_web/endpoint.ex"
  @web_module_path "lib/test_app_web.ex"
  @root_layout_path "lib/test_app_web/components/layouts/root.html.heex"
  @app_js_path "assets/js/app.js"
  @config_path "config/config.exs"
  @mix_path "mix.exs"
  @router_path "lib/test_app_web/router.ex"
  @hello_live_path "lib/test_app_web/live/hello_local_live.ex"

  defp installed do
    [app_name: :test_app]
    |> phx_test_project()
    |> Igniter.compose_task("llv.install", [])
  end

  describe "inject_endpoint" do
    test "adds socket, plug and security-headers defp to the endpoint" do
      installed()
      |> assert_has_patch(@endpoint_path, """
      + |plug(:put_wasm_security_headers)
      """)
      |> assert_has_patch(@endpoint_path, """
      + |  socket("/llv_socket", LocalLiveView.Socket,
      + |    websocket: [connect_info: [session: @session_options]]
      + |  )
      """)
      |> assert_has_patch(@endpoint_path, """
      + |defp put_wasm_security_headers(conn, _opts) do
      """)
      |> assert_has_patch(@endpoint_path, """
      + |    |> put_resp_header("cross-origin-opener-policy", "same-origin")
      """)
    end
  end

  describe "inject_web_module" do
    test "adds `import LocalLiveView.Component` to html_helpers" do
      installed()
      |> assert_has_patch(@web_module_path, """
      + |      import LocalLiveView.Component
      """)
    end
  end

  describe "inject_root_layout" do
    test "swaps type=text/javascript -> type=module on the app.js script tag" do
      installed()
      |> assert_has_patch(@root_layout_path, """
      + |    <script defer phx-track-static type="module" src={~p"/assets/js/app.js"}>
      """)
    end
  end

  describe "inject_app_js" do
    test "adds LLVEngine.create() import + call after liveSocket.connect()" do
      installed()
      |> assert_has_patch(@app_js_path, """
      + |import { LLVEngine } from "local_live_view";
      """)
      |> assert_has_patch(@app_js_path, """
      + |await LLVEngine.create(liveSocket, { Socket, bundlePaths: ["/assets/js/wasm/bundle.avm"] });
      """)
    end
  end

  describe "inject_esbuild_format" do
    test "adds --format=esm next to --bundle in config/config.exs" do
      installed()
      |> assert_has_patch(@config_path, """
      + |      ~w(js/app.js --bundle --format=esm
      """)
    end
  end

  describe "inject_setup_alias" do
    test "inserts llv.build right after deps.get in the setup alias" do
      installed()
      |> assert_has_patch(@mix_path, """
      + |      setup: ["deps.get", "llv.build", "ecto.setup", "assets.setup", "assets.build"],
      """)
    end
  end

  describe "generate_local_project" do
    test "creates all the local/* template files" do
      installed()
      |> assert_creates("local/mix.exs")
      |> assert_creates("local/config/config.exs")
      |> assert_creates("local/.formatter.exs")
      |> assert_creates("local/lib/local/application.ex")
      |> assert_creates("local/lib/hello_local.ex")
    end

    test "skips when local/mix.exs already exists" do
      [app_name: :test_app, files: %{"local/mix.exs" => "# already here"}]
      |> phx_test_project()
      |> Igniter.compose_task("llv.install", [])
      |> assert_unchanged("local/mix.exs")
    end
  end

  describe "generate_hello_live_view" do
    test "creates a LiveView that renders the HelloLocal local live view" do
      installed()
      |> assert_creates(@hello_live_path, """
      defmodule TestAppWeb.HelloLocalLive do
        use TestAppWeb, :live_view

        @impl true
        def render(assigns) do
          ~H\"\"\"
          <.local_live_view view="HelloLocal" />
          \"\"\"
        end
      end
      """)
    end

    test "skips when the module already exists" do
      existing = "defmodule TestAppWeb.HelloLocalLive do\nend\n"

      [app_name: :test_app, files: %{@hello_live_path => existing}]
      |> phx_test_project()
      |> Igniter.compose_task("llv.install", [])
      |> assert_unchanged(@hello_live_path)
    end
  end

  describe "inject_hello_route" do
    test "adds a live route to the browser scope" do
      installed()
      |> assert_has_patch(@router_path, """
        |    get("/", PageController, :home)
      + |    live "/hello_local", HelloLocalLive
      """)
    end
  end

  describe "idempotency" do
    test "re-running the installer does not re-inject anything" do
      installed()
      |> apply_igniter!()
      |> Igniter.compose_task("llv.install", [])
      |> assert_unchanged([
        @endpoint_path,
        @web_module_path,
        @root_layout_path,
        @app_js_path,
        @config_path,
        @mix_path,
        @router_path,
        @hello_live_path
      ])
    end
  end
end
