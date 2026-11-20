defmodule LivebookWasm.Plug do
  @moduledoc """
  Serves a plug, Livebook's endpoint, to JavaScript in the page.

  There's no network or HTTP server: JavaScript sends requests to the VM, and
  each runs the plug in its own process, like a connection in a web server.
  `LivebookWasm.Application` starts it after Livebook, whose endpoint has
  `server: false`.

  ## Requests

  JavaScript calls the server, registered as `:livebook_wasm_plug` by default:

  ```javascript
  const { status, headers, body } = await popcorn.call("livebook_wasm_plug", {
    method: "POST",
    url: "http://localhost/notes?draft=1",
    headers: [["content-type", "application/x-www-form-urlencoded"]],
    body: "title=Hello",
  });
  ```

  Headers are `[name, value]` pairs, including `set-cookie`: responses built
  in JavaScript can't set cookies, so the page has to keep them and send them
  back in `cookie` headers. Bodies are strings, or `Uint8Array`s when they
  aren't valid UTF-8. A URL without a host takes it from the `host` header.

  ## WebSockets

  An upgrade to a WebSocket, as in Phoenix's sockets, is answered with
  `{status: 101, socket, id}`, and the request's process stays as the socket.
  JavaScript sends it frames with `popcorn.send(socket, {text: data})` or
  `{binary: data}` and closes it with `{close: code}`. The socket's frames
  arrive in `popcorn.onEvent()` as `{popcorn_socket: id, text: data}` or
  `{popcorn_socket: id, binary: data}`, and its close as
  `{popcorn_socket: id, close: code}`.

  ## Options

    * `:plug` - the plug, as a module or a `{module, options}` tuple. Required.
    * `:name` - the name JavaScript calls. Default: `:livebook_wasm_plug`.
  """
  use GenServer

  require Logger

  alias LivebookWasm.Plug.Adapter

  @doc false
  def start_link(opts) do
    {plug, plug_opts} =
      case Keyword.fetch!(opts, :plug) do
        {plug, plug_opts} -> {plug, plug_opts}
        plug -> {plug, []}
      end

    name = Keyword.get(opts, :name, :livebook_wasm_plug)
    GenServer.start_link(__MODULE__, {plug, plug_opts}, name: name)
  end

  @impl true
  def init({plug, plug_opts}), do: {:ok, {plug, plug.init(plug_opts)}}

  @impl true
  def handle_call(%{"method" => _, "url" => _} = request, from, plug) do
    spawn(fn -> serve(plug, request, from) end)
    {:noreply, plug}
  end

  defp serve(plug, request, from) do
    method = String.upcase(request["method"])
    headers = for [name, value] <- request["headers"] || [], do: {String.downcase(name), value}
    uri = uri(request["url"], headers)
    body = request["body"] || ""
    conn = Adapter.conn(method, uri, headers, body)

    case run(plug, conn) do
      {:upgrade, {websock, state, opts}} ->
        id = System.unique_integer([:positive])
        GenServer.reply(from, %{status: 101, socket: self(), id: id})
        LivebookWasm.Plug.Socket.run(websock, state, opts, id)

      {:response, status, resp_headers, resp_body} ->
        resp_headers = for {name, value} <- resp_headers, do: [name, value]
        GenServer.reply(from, %{status: status, headers: resp_headers, body: resp_body})
    end
  end

  defp run(plug, conn) do
    {Adapter, %{ref: ref}} = conn.adapter

    case call(plug, conn) do
      {:ok, %Plug.Conn{state: :upgraded, adapter: {Adapter, %{upgrade: upgrade}}}} ->
        {:upgrade, upgrade}

      _result ->
        receive do
          {^ref, {:response, status, headers, body}} ->
            {:response, status, headers, body}

          {^ref, {:chunked, status, headers}} ->
            {:response, status, headers, chunks(ref, [])}
        after
          0 -> {:response, 500, [], ""}
        end
    end
  end

  defp chunks(ref, acc) do
    receive do
      {^ref, {:chunk, data}} -> chunks(ref, [acc | data])
    after
      0 -> IO.iodata_to_binary(acc)
    end
  end

  defp call({plug, plug_opts}, conn) do
    {:ok, plug.call(conn, plug_opts)}
  catch
    kind, reason ->
      # Web servers log the requests that crash, which Phoenix leaves to them
      Logger.error(Exception.format(kind, reason, __STACKTRACE__))
      :error
  end

  defp uri(url, headers) do
    uri = URI.parse(url)

    uri =
      if uri.host do
        uri
      else
        {_, host} = List.keyfind(headers, "host", 0, {"host", "localhost"})
        base = URI.parse("http://" <> host)
        %{uri | scheme: uri.scheme || "http", host: base.host, port: base.port}
      end

    %{uri | path: uri.path || "/"}
  end
end
