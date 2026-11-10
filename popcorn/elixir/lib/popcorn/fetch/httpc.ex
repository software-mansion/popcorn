defmodule Popcorn.Fetch.HTTPC do
  @moduledoc false

  # Bridge between :httpc and Popcorn.Fetch. :httpc patch calls it.

  @compile {:no_warn_undefined, :httpd_util}

  def request(method, request, http_options, options, _profile) do
    with {:ok, args} <- fetch_args(method, request) do
      timeout = Keyword.get(http_options, :timeout, :infinity)

      if Keyword.get(options, :sync, true) do
        respond_sync(args, timeout, options)
      else
        respond_async(args, timeout, options)
      end
    end
  end

  # A request's id is its process's alias, which goes away with the process, so
  # the id of a finished request affects nothing
  def cancel_request(request_id, _profile) do
    if is_reference(request_id), do: :erlang.exit(request_id, :kill)
    :ok
  end

  def stream_next(pid) do
    send(pid, {__MODULE__, :stream_next})
    :ok
  end

  defp fetch_args(method, {url, headers}) do
    {:ok, %{method: method(method), url: url(url), headers: headers(headers)}}
  end

  defp fetch_args(method, {url, headers, content_type, body})
       when is_binary(body) or is_list(body) do
    content_type_header = {"content-type", IO.iodata_to_binary(content_type)}

    {:ok,
     %{
       method: method(method),
       url: url(url),
       headers: [content_type_header | headers(headers)],
       body: IO.iodata_to_binary(body)
     }}
  end

  defp fetch_args(_method, {_url, _headers, _content_type, _body}) do
    {:error, {:not_supported, :body_generator}}
  end

  defp fetch_args(_method, request), do: {:error, {:bad_request, request}}

  defp method(method), do: method |> Atom.to_string() |> String.upcase()

  defp url(url), do: IO.chardata_to_string(url)

  defp headers(headers) do
    for {name, value} <- headers, do: {IO.iodata_to_binary(name), IO.iodata_to_binary(value)}
  end

  defp respond_sync(args, timeout, options) do
    with {:ok, status, headers, conn} <- open(args, timeout) do
      file = Keyword.get(options, :stream)

      if is_list(file) and status in [200, 206] do
        save(file, conn)
      else
        with {:ok, body} <- Popcorn.Fetch.read_body(conn, :infinity),
             do: {:ok, result(status, headers, body, options)}
      end
    end
  end

  defp respond_async(args, timeout, options) do
    owner = self()
    start_ref = make_ref()

    {:ok, _pid} =
      Task.start(fn ->
        id = Process.monitor(owner, alias: :explicit_unalias)
        send(owner, {start_ref, id})

        async = %{
          id: id,
          options: options,
          stream: Keyword.get(options, :stream),
          deliver: receiver(Keyword.get(options, :receiver, owner))
        }

        respond(async, open(args, timeout))
      end)

    receive do
      {^start_ref, id} -> {:ok, id}
    end
  end

  defp open(args, timeout) do
    with {:ok, conn} <- Popcorn.Fetch.start(args, timeout: timeout),
         {:head, status, headers} <- Popcorn.Fetch.next(conn, :infinity) do
      headers =
        for {name, value} <- headers,
            do: {:erlang.binary_to_list(name), :erlang.binary_to_list(value)}

      {:ok, status, headers, conn}
    end
  end

  defp receiver(pid) when is_pid(pid), do: fn reply -> send(pid, {:http, reply}) end
  defp receiver(fun) when is_function(fun, 1), do: fun

  defp receiver({module, function, args}) do
    fn reply -> apply(module, function, [reply | args]) end
  end

  # httpc streams only 200 and 206 responses
  defp respond(%{stream: :self} = async, {:ok, status, headers, conn})
       when status in [200, 206] do
    async.deliver.({async.id, :stream_start, headers})
    stream(async, headers, conn)
  end

  defp respond(%{stream: {:self, :once}} = async, {:ok, status, headers, conn})
       when status in [200, 206] do
    async.deliver.({async.id, :stream_start, headers, self()})
    stream(async, headers, conn)
  end

  defp respond(%{stream: file} = async, {:ok, status, _headers, conn})
       when is_list(file) and status in [200, 206] do
    reply = with {:ok, saved} <- save(file, conn), do: saved
    async.deliver.({async.id, reply})
  end

  defp respond(async, {:ok, status, headers, conn}) do
    reply =
      with {:ok, body} <- Popcorn.Fetch.read_body(conn, :infinity),
           do: result(status, headers, body, async.options)

    async.deliver.({async.id, reply})
  end

  defp respond(async, {:error, _reason} = error) do
    async.deliver.({async.id, error})
  end

  # The parts as they arrive
  defp stream(async, headers, conn) do
    await_next(async)

    case Popcorn.Fetch.next(conn, :infinity) do
      {:data, part} ->
        async.deliver.({async.id, :stream, part})
        stream(async, headers, conn)

      :done ->
        async.deliver.({async.id, :stream_end, headers})

      {:error, _reason} = error ->
        async.deliver.({async.id, error})
    end
  end

  # With {self, once}, each message waits for httpc:stream_next/1
  defp await_next(%{stream: {:self, :once}, id: monitor}) do
    receive do
      {__MODULE__, :stream_next} -> :ok
      {:DOWN, ^monitor, :process, _pid, _reason} -> exit(:normal)
    end
  end

  defp await_next(_async), do: :ok

  # The parts as they arrive
  defp save(file, conn) do
    case File.open(file, [:write, :binary], &write(&1, conn)) do
      {:ok, :ok} ->
        {:ok, :saved_to_file}

      {:ok, error} ->
        error

      error ->
        Popcorn.Fetch.close(conn)
        error
    end
  end

  defp write(device, conn) do
    case Popcorn.Fetch.next(conn, :infinity) do
      {:data, data} ->
        case IO.binwrite(device, data) do
          :ok ->
            write(device, conn)

          error ->
            Popcorn.Fetch.close(conn)
            error
        end

      :done ->
        :ok

      {:error, _reason} = error ->
        error
    end
  end

  defp result(status, headers, body, options) do
    body =
      if Keyword.get(options, :body_format) == :binary,
        do: body,
        else: :erlang.binary_to_list(body)

    if Keyword.get(options, :full_result, true) do
      {{~c"HTTP/1.1", status, :httpd_util.reason_phrase(status)}, headers, body}
    else
      {status, body}
    end
  end
end
