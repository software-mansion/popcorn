defmodule LivebookWasm.Plug.Adapter do
  @moduledoc """
  The `Plug.Conn.Adapter` of `LivebookWasm.Plug`, for connections from
  JavaScript.

  The request process is the connection: the adapter sends it the response as
  messages, which `LivebookWasm.Plug` relays to JavaScript. A WebSocket upgrade
  is kept in the adapter's state, in its `upgrade` field.
  """
  @behaviour Plug.Conn.Adapter

  @already_sent Plug.Conn.Adapter.already_sent()

  @enforce_keys [:owner, :ref, :method, :req_body, :port]
  defstruct [:owner, :ref, :method, :req_body, :port, :upgrade]

  @typedoc false
  @type t :: %__MODULE__{
          owner: pid(),
          ref: reference(),
          method: String.t(),
          req_body: binary(),
          port: :inet.port_number(),
          upgrade: nil | {module(), term(), keyword()}
        }

  @doc false
  # The address of the page: requests come from the browser itself
  def peer_address, do: {127, 0, 0, 1}

  @doc false
  def conn(method, %URI{} = uri, req_headers, req_body) do
    state = %__MODULE__{
      owner: self(),
      ref: make_ref(),
      method: method,
      req_body: req_body,
      port: uri.port
    }

    Plug.Conn.Adapter.conn({__MODULE__, state}, method, uri, peer_address(), req_headers)
  end

  @impl true
  def send_resp(state, status, headers, body) do
    body = if state.method == "HEAD", do: "", else: IO.iodata_to_binary(body)
    sent(state, {:response, status, headers, body})
    {:ok, nil, state}
  end

  @impl true
  def send_file(state, status, headers, path, offset, length) do
    body =
      if state.method == "HEAD" do
        ""
      else
        length = if length == :all, do: File.stat!(path).size - offset, else: length

        File.open!(path, [:read, :binary, :raw], fn file ->
          case :file.pread(file, offset, length) do
            {:ok, data} -> data
            :eof -> ""
          end
        end)
      end

    sent(state, {:response, status, headers, body})
    {:ok, nil, state}
  end

  @impl true
  def send_chunked(state, status, headers) do
    sent(state, {:chunked, status, headers})
    {:ok, nil, state}
  end

  @impl true
  def chunk(%{method: "HEAD"}, _body), do: :ok

  def chunk(state, body) do
    send(state.owner, {state.ref, {:chunk, IO.iodata_to_binary(body)}})
    :ok
  end

  @impl true
  def read_req_body(state, opts) do
    length = Keyword.get(opts, :length, 8_000_000)

    case state.req_body do
      <<data::binary-size(^length), rest::binary>> when rest != "" ->
        {:more, data, %{state | req_body: rest}}

      data ->
        {:ok, data, %{state | req_body: ""}}
    end
  end

  # Informational responses can't reach a page through JavaScript
  @impl true
  def inform(_state, _status, _headers), do: {:error, :not_supported}

  @impl true
  def upgrade(state, :websocket, {_websock, _state, _opts} = upgrade) do
    {:ok, %{state | upgrade: upgrade}}
  end

  def upgrade(_state, _protocol, _opts), do: {:error, :not_supported}

  @impl true
  def get_peer_data(_state), do: %{address: peer_address(), port: 0, ssl_cert: nil}

  @impl true
  def get_sock_data(state), do: %{address: peer_address(), port: state.port}

  @impl true
  def get_ssl_data(_state), do: nil

  @impl true
  def get_http_protocol(_state), do: :"HTTP/1.1"

  defp sent(state, response) do
    send(state.owner, @already_sent)
    send(state.owner, {state.ref, response})
  end
end
