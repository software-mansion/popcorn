defmodule Popcorn.FetchTest do
  use ExUnit.Case, async: false

  alias Popcorn.Fetch
  alias Popcorn.Wasm.FakeBridge

  # Not UTF-8: catches any path that routes bytes through a JS string.
  @binary <<255, 0, 65, 254>>

  setup do
    Application.put_env(:popcorn_otp, :wasm_bridge, FakeBridge)
    on_exit(fn -> Application.delete_env(:popcorn_otp, :wasm_bridge) end)
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

  describe "run/1 as a Req adapter" do
    test "translates requests and composes with Req steps" do
      FakeBridge.stub_fetch(%{
        status: 200,
        headers: [{"content-type", "application/json"}],
        chunks: [~s({"a":), ~s(1})]
      })

      assert %{status: 200, body: %{"a" => 1}} =
               Req.post!("https://example.test/x",
                 adapter: Fetch,
                 body: "payload",
                 headers: [{"x-test", "1"}]
               )

      assert %{method: "POST", url: "https://example.test/x", body: encoded_body} =
               FakeBridge.fetch_request()

      assert ["x-test", "1"] in FakeBridge.fetch_request().headers
      assert Base.decode64!(encoded_body) == "payload"
    end

    test "returns a TransportError so retry and error steps behave normally" do
      FakeBridge.stub_fetch(%{error: "TypeError: Failed to fetch"})

      assert {:error, %Req.TransportError{reason: {:fetch, _}}} =
               Req.get("https://example.test/", adapter: Fetch, retry: false)
    end

    test "streams into functions and collectables" do
      FakeBridge.stub_fetch(%{chunks: ["a", "b", "c"]})

      response =
        Req.get!("https://example.test/",
          adapter: Fetch,
          into: fn {:data, data}, {req, resp} ->
            {:cont, {req, update_in(resp.body, &(&1 <> data))}}
          end
        )

      assert response.body == "abc"

      FakeBridge.stub_fetch(%{chunks: ["a", "b", "c"]})

      halted =
        Req.get!("https://example.test/",
          adapter: Fetch,
          into: fn {:data, data}, {req, resp} ->
            {:halt, {req, update_in(resp.body, &(&1 <> data))}}
          end
        )

      assert halted.body == "a"
      assert FakeBridge.fetch_aborted?()

      FakeBridge.stub_fetch(%{chunks: ["x", "y"]})

      response = Req.get!("https://example.test/", adapter: Fetch, into: [])
      assert response.body == ["x", "y"]
    end

    test "streams into the mailbox and aborts receive timeouts" do
      FakeBridge.stub_fetch(%{status: 200, chunks: ["one", "two"]})

      response = Req.get!("https://example.test/", adapter: Fetch, into: :self)
      assert response.status == 200
      assert Enum.to_list(response.body) == ["one", "two"]

      FakeBridge.stub_fetch(%{chunks: ["one"], done: false})

      response =
        Req.get!("https://example.test/",
          adapter: Fetch,
          into: :self,
          receive_timeout: 10
        )

      assert_raise Req.TransportError, fn -> Enum.to_list(response.body) end
      assert FakeBridge.fetch_aborted?()
    end
  end
end
