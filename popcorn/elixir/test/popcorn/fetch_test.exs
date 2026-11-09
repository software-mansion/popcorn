defmodule Popcorn.FetchTest do
  use ExUnit.Case, async: false

  alias Popcorn.Fetch
  alias Popcorn.JS.FakeBridge

  # Not UTF-8: catches any path that routes bytes through a JS string.
  @binary <<255, 0, 65, 254>>

  setup do
    Application.put_env(:popcorn, :wasm_bridge, FakeBridge)
    on_exit(fn -> Application.delete_env(:popcorn, :wasm_bridge) end)
  end

  describe "compilation" do
    test "compiles without Req, and installs the adapter once Req is there" do
      fixture = Path.expand("../fixtures/fetch_without_req_helper.exs", __DIR__)
      req_dir = to_string(:code.lib_dir(:req))
      {output, status} = System.cmd("elixir", [fixture, req_dir], stderr_to_stdout: true)

      assert status == 0, output
      assert output == ""
    end

    test "installs the compiled adapter and preserves overrides" do
      fixture = Path.expand("../fixtures/fetch_with_req_helper.exs", __DIR__)
      paths = Enum.flat_map(:code.get_path(), fn path -> ["-pa", to_string(path)] end)
      {output, status} = System.cmd("elixir", paths ++ [fixture], stderr_to_stdout: true)

      assert status == 0, output
      assert output == ""
    end
  end

  describe "request/2" do
    test "translates requests and responses without changing bytes" do
      FakeBridge.stub_fetch(%{
        status: 201,
        headers: [{"content-type", "text/plain"}],
        chunks: [<<255, 0>>, <<65, 254>>]
      })

      result =
        Fetch.request(%{
          method: "POST",
          url: "https://example.test/things",
          headers: [{"accept", "text/plain"}],
          body: @binary
        })

      assert {:ok,
              %{
                status: 201,
                headers: [{"content-type", "text/plain"}],
                body: @binary
              }} = result

      assert %{
               method: "POST",
               url: "https://example.test/things",
               headers: [["accept", "text/plain"]],
               body: encoded_body
             } = FakeBridge.fetch_request()

      assert Base.decode64!(encoded_body) == @binary
    end

    # The browser decompresses bodies, which these headers describe compressed
    test "leaves out the compressed body's encoding and length" do
      FakeBridge.stub_fetch(%{
        status: 200,
        headers: [
          {"content-type", "application/json"},
          {"content-encoding", "gzip"},
          {"content-length", "3"}
        ],
        chunks: [~s({"a":1})]
      })

      assert {:ok, %{headers: [{"content-type", "application/json"}], body: ~s({"a":1})}} =
               Fetch.request(%{method: "GET", url: "https://example.test/x"})
    end

    test "omits the body key entirely when there is none" do
      FakeBridge.stub_fetch(%{chunks: []})

      assert {:ok, %{body: ""}} =
               Fetch.request(%{
                 method: "GET",
                 url: "https://example.test/"
               })

      refute Map.has_key?(FakeBridge.fetch_request(), :body)
    end

    test "reports browser failures and aborts timeouts" do
      FakeBridge.stub_fetch(%{error: "TypeError: Failed to fetch"})

      assert {:error, {:fetch, message}} =
               Fetch.request(%{method: "GET", url: "https://example.test/"})

      assert message =~ "TypeError: Failed to fetch"
      assert message =~ "CORS"

      FakeBridge.stub_fetch(:never_replies)

      assert {:error, :timeout} =
               Fetch.request(%{method: "GET", url: "https://example.test/"}, timeout: 10)

      assert FakeBridge.fetch_aborted?()
    end
  end

  describe "Req adapter" do
    if Code.ensure_loaded?(Req.Adapter) do
      test "threads request and response streams through the callback" do
        FakeBridge.stub_fetch(%{
          status: 201,
          headers: [{"x-fetch", "streamed"}],
          chunks: ["one", "two"]
        })

        body = fn
          [] -> {:data, "pay", [:body_started]}
          [:body_started] -> {:done, "load", [:body_done, :body_started]}
        end

        request =
          Req.new(
            method: :post,
            url: "https://example.test/x",
            headers: [{"x-request", "yes"}],
            body: body
          )

        callback = fn event, response, acc, state ->
          {:ok, response, [event | acc], [event | state]}
        end

        assert {:ok, response, acc, state} = Fetch.stream(request, [], callback, [])
        assert %Req.Response{status: 201, request: ^request} = response
        assert ["streamed"] = Req.Response.get_header(response, "x-fetch")

        assert [
                 {:data, "two"},
                 {:data, "one"},
                 {:headers, [{"x-fetch", "streamed"}]},
                 {:status, 201},
                 :body_done,
                 :body_started
               ] = acc

        assert [
                 {:data, "two"},
                 {:data, "one"},
                 {:headers, [{"x-fetch", "streamed"}]},
                 {:status, 201}
               ] = state

        assert %{body: encoded_body, headers: headers} = FakeBridge.fetch_request()
        assert Base.decode64!(encoded_body) == "payload"
        assert ["x-request", "yes"] in headers
      end

      test "returns request body errors before starting fetch" do
        error = RuntimeError.exception("upload failed")

        request =
          Req.new(url: "https://example.test/", body: fn :start -> {:error, error, :failed} end)

        callback = fn event, response, acc, state -> {:ok, response, [event | acc], state} end

        assert {{:error, ^error}, response, :failed, [:wrapper]} =
                 Fetch.stream(request, :start, callback, [:wrapper])

        assert %Req.Response{status: nil, body: nil, request: ^request} = response
        assert is_nil(FakeBridge.fetch_request())
      end
    end

    test "supports the legacy run contract" do
      FakeBridge.stub_fetch(%{status: 201, chunks: ["legacy"]})
      request = Req.new(method: :post, url: "https://example.test/", body: "payload")

      assert {^request, %Req.Response{status: 201, body: "legacy"}} = Fetch.run(request)
      assert %{body: encoded_body} = FakeBridge.fetch_request()
      assert Base.decode64!(encoded_body) == "payload"
    end

    test "translates requests and composes with Req steps" do
      FakeBridge.stub_fetch(%{
        status: 200,
        headers: [{"content-type", "application/json"}],
        chunks: [~s({"a":), ~s(1})]
      })

      assert %{status: 200, body: %{"a" => 1}} =
               Req.post!("https://example.test/x",
                 adapter: adapter(),
                 body: "payload",
                 headers: [{"x-test", "1"}]
               )

      assert %{method: "POST", url: "https://example.test/x", body: encoded_body} =
               FakeBridge.fetch_request()

      assert ["x-test", "1"] in FakeBridge.fetch_request().headers
      assert Base.decode64!(encoded_body) == "payload"
    end

    test "decodes bodies that the browser has decompressed" do
      FakeBridge.stub_fetch(%{
        status: 200,
        headers: [{"content-type", "application/json"}, {"content-encoding", "br"}],
        chunks: [~s({"a":), ~s(1})]
      })

      assert %{status: 200, body: %{"a" => 1}} =
               Req.get!("https://example.test/x", adapter: adapter())
    end

    test "returns a TransportError so retry and error steps behave normally" do
      FakeBridge.stub_fetch(%{error: "TypeError: Failed to fetch"})

      assert {:error, %Req.TransportError{reason: {:fetch, _}}} =
               Req.get("https://example.test/", adapter: adapter(), retry: false)
    end

    test "streams into functions and collectables" do
      FakeBridge.stub_fetch(%{chunks: ["a", "b", "c"]})

      response =
        Req.get!("https://example.test/",
          adapter: adapter(),
          into: fn {:data, data}, {req, resp} ->
            {:cont, {req, update_in(resp.body, &(&1 <> data))}}
          end
        )

      assert response.body == "abc"

      FakeBridge.stub_fetch(%{chunks: ["a", "b", "c"]})

      halted =
        Req.get!("https://example.test/",
          adapter: adapter(),
          into: fn {:data, data}, {req, resp} ->
            {:halt, {req, update_in(resp.body, &(&1 <> data))}}
          end
        )

      assert halted.body == "a"
      assert FakeBridge.fetch_aborted?()

      FakeBridge.stub_fetch(%{chunks: ["x", "y"]})

      response = Req.get!("https://example.test/", adapter: adapter(), into: [])
      assert response.body == ["x", "y"]
    end

    test "streams into the mailbox and aborts receive timeouts" do
      FakeBridge.stub_fetch(%{status: 200, chunks: ["one", "two"]})

      response = Req.get!("https://example.test/", adapter: adapter(), into: :self)
      assert response.status == 200
      assert Enum.to_list(response.body) == ["one", "two"]

      FakeBridge.stub_fetch(%{chunks: ["one"], done: false})

      response =
        Req.get!("https://example.test/",
          adapter: adapter(),
          into: :self,
          receive_timeout: 10
        )

      assert_raise Req.TransportError, fn -> Enum.to_list(response.body) end
      assert FakeBridge.fetch_aborted?()
    end
  end

  defp adapter do
    Fetch.adapter()
  end
end
