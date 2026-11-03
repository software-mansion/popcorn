defmodule Popcorn.PlugTest do
  # Sockets push to JavaScript through the bridge, which is global
  use ExUnit.Case, async: false

  import ExUnit.CaptureLog

  defmodule Bridge do
    @moduledoc false
    # Hands what Elixir sends to JavaScript to the test process
    def send(message) do
      Kernel.send(Application.fetch_env!(:popcorn, :plug_test_pid), {:js, message})
      :ok
    end
  end

  defmodule Echo do
    @moduledoc false
    @behaviour WebSock

    @impl true
    def init(test_pid) do
      send(test_pid, {:init, self()})
      {:ok, test_pid}
    end

    @impl true
    def handle_in({"stop", opcode: :text}, test_pid), do: {:stop, :normal, test_pid}

    def handle_in({"stop with detail", opcode: :text}, test_pid),
      do: {:stop, :done, {4000, "bye"}, [{:text, "last"}], test_pid}

    def handle_in({data, opcode: opcode}, test_pid),
      do: {:push, {opcode, ["echo:", data]}, test_pid}

    @impl true
    def handle_info(message, test_pid),
      do: {:reply, :ok, [{:text, "info:#{inspect(message)}"}, {:ping, nil}], test_pid}

    @impl true
    def terminate(reason, test_pid), do: send(test_pid, {:terminate, reason})
  end

  defmodule Router do
    @moduledoc false
    @behaviour Plug

    import Plug.Conn

    @upgrade_headers [
      {"upgrade", "websocket"},
      {"connection", "Upgrade"},
      {"sec-websocket-key", "dGhlIHNhbXBsZSBub25jZQ=="},
      {"sec-websocket-version", "13"}
    ]
    def upgrade_headers, do: for({name, value} <- @upgrade_headers, do: [name, value])

    @impl true
    def init(test_pid), do: test_pid

    @impl true
    def call(%{request_path: "/hello"} = conn, _test_pid) do
      %{host: host, port: port, scheme: scheme, query_string: query} = conn
      %{address: address} = get_peer_data(conn)

      conn
      |> put_resp_header("x-peer", :inet.ntoa(address) |> to_string())
      |> send_resp(200, "#{conn.method} #{scheme}://#{host}:#{port} #{query}")
    end

    def call(%{request_path: "/body"} = conn, _test_pid) do
      {reads, body, conn} = read_all(conn, 0, [])
      send_resp(conn, 200, "#{reads} reads: #{body}")
    end

    def call(%{request_path: "/file"} = conn, _test_pid) do
      %{"path" => path} = fetch_query_params(conn).query_params
      send_file(conn, 200, path, 2, :all)
    end

    def call(%{request_path: "/chunked"} = conn, _test_pid) do
      conn = send_chunked(conn, 201)
      {:ok, conn} = chunk(conn, "a")
      {:ok, conn} = chunk(conn, ["b", ?c])
      conn
    end

    def call(%{request_path: "/crash-after-send"} = conn, _test_pid) do
      send_resp(conn, 404, "Not found")
      raise "after sending"
    end

    def call(%{request_path: "/crash"}, _test_pid), do: raise("before sending")
    def call(%{request_path: "/no-response"} = conn, _test_pid), do: conn

    def call(%{request_path: "/upgrade"} = conn, test_pid) do
      timeout = conn |> fetch_query_params() |> Map.get(:query_params) |> Map.get("timeout")
      opts = if timeout, do: [timeout: String.to_integer(timeout)], else: []
      upgrade_adapter(conn, :websocket, {Echo, test_pid, opts})
    end

    def call(%{request_path: "/websock"} = conn, test_pid) do
      WebSockAdapter.upgrade(conn, Echo, test_pid, [])
    end

    defp read_all(conn, reads, acc) do
      case read_body(conn, length: 3) do
        {:more, data, conn} -> read_all(conn, reads + 1, [acc | data])
        {:ok, data, conn} -> {reads + 1, IO.iodata_to_binary([acc | data]), conn}
      end
    end
  end

  setup context do
    Application.put_env(:popcorn, :wasm_bridge, Bridge)
    Application.put_env(:popcorn, :plug_test_pid, self())

    on_exit(fn ->
      Application.delete_env(:popcorn, :wasm_bridge)
      Application.delete_env(:popcorn, :plug_test_pid)
    end)

    name = :"#{__MODULE__}.#{context.test}"
    start_supervised!({Popcorn.Plug, plug: {Router, self()}, name: name})
    {:ok, name: name}
  end

  defp request(name, method, url, opts \\ []) do
    GenServer.call(name, %{
      "method" => method,
      "url" => url,
      "headers" => Keyword.get(opts, :headers, []),
      "body" => Keyword.get(opts, :body)
    })
  end

  describe "requests" do
    test "are served by the plug", %{name: name} do
      assert %{status: 200, headers: headers, body: body} =
               request(name, "get", "https://example.com:8443/hello?a=1")

      assert body == "GET https://example.com:8443 a=1"
      assert ["x-peer", "127.0.0.1"] in headers
    end

    test "without a host take it from the host header", %{name: name} do
      assert %{body: "GET http://localhost:4000 "} =
               request(name, "GET", "/hello", headers: [["Host", "localhost:4000"]])

      assert %{body: "GET http://localhost:80 "} = request(name, "GET", "/hello")
    end

    test "have bodies read in parts", %{name: name} do
      assert %{body: "3 reads: abcdefg"} = request(name, "POST", "/body", body: "abcdefg")
      assert %{body: "1 reads: "} = request(name, "POST", "/body")
    end

    test "with the HEAD method get no body", %{name: name} do
      assert %{status: 200, body: ""} = request(name, "HEAD", "/hello")
    end

    @tag :tmp_dir
    test "can be answered with files", %{name: name, tmp_dir: tmp_dir} do
      path = Path.join(tmp_dir, "file.txt")
      File.write!(path, "0123456789")
      query = URI.encode_query(%{path: path})
      assert %{status: 200, body: "23456789"} = request(name, "GET", "/file?#{query}")
    end

    test "can be answered in chunks", %{name: name} do
      assert %{status: 201, body: "abc"} = request(name, "GET", "/chunked")
    end

    test "that crash are logged", %{name: name} do
      log =
        capture_log(fn ->
          assert %{status: 404, body: "Not found"} = request(name, "GET", "/crash-after-send")
          assert %{status: 500, body: ""} = request(name, "GET", "/crash")
        end)

      assert log =~ "after sending"
      assert log =~ "before sending"
    end

    test "that the plug doesn't answer get 500", %{name: name} do
      assert %{status: 500} = request(name, "GET", "/no-response")
    end
  end

  describe "WebSockets" do
    test "exchange frames with JavaScript", %{name: name} do
      assert %{status: 101, socket: socket, id: id} = request(name, "GET", "/upgrade")
      assert_receive {:init, ^socket}

      send(socket, {:wasm, %{"text" => "hi"}})
      assert_receive {:js, %{popcorn_socket: ^id, text: "echo:hi"}}

      send(socket, {:wasm, %{"binary" => <<1, 2>>}})
      assert_receive {:js, %{popcorn_socket: ^id, binary: "echo:" <> <<1, 2>>}}

      send(socket, :tick)
      assert_receive {:js, %{popcorn_socket: ^id, text: "info::tick"}}

      send(socket, {:wasm, %{"text" => "stop"}})
      assert_receive {:js, %{popcorn_socket: ^id, close: 1000}}
      assert_receive {:terminate, :normal}
    end

    test "are closed with the handler's detail", %{name: name} do
      assert %{status: 101, socket: socket, id: id} = request(name, "GET", "/upgrade")

      send(socket, {:wasm, %{"text" => "stop with detail"}})
      assert_receive {:js, %{popcorn_socket: ^id, text: "last"}}
      assert_receive {:js, %{popcorn_socket: ^id, close: 4000}}
      assert_receive {:terminate, {:error, :done}}
    end

    test "are closed by JavaScript", %{name: name} do
      assert %{status: 101, socket: socket} = request(name, "GET", "/upgrade")
      ref = Process.monitor(socket)

      send(socket, {:wasm, %{"close" => 1000}})
      assert_receive {:terminate, :remote}
      assert_receive {:DOWN, ^ref, :process, ^socket, :normal}
    end

    test "time out when idle", %{name: name} do
      assert %{status: 101, id: id} = request(name, "GET", "/upgrade?timeout=10")
      assert_receive {:js, %{popcorn_socket: ^id, close: 1000}}
      assert_receive {:terminate, :timeout}
    end

    test "upgrade through WebSockAdapter", %{name: name} do
      headers = [["host", "localhost"] | Router.upgrade_headers()]

      assert %{status: 101, socket: socket, id: id} =
               request(name, "GET", "/websock", headers: headers)

      send(socket, {:wasm, %{"text" => "hi"}})
      assert_receive {:js, %{popcorn_socket: ^id, text: "echo:hi"}}
    end
  end
end
