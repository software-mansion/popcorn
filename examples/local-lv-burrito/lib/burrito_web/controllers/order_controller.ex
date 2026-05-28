defmodule BurritoWeb.OrderController do
  use BurritoWeb, :controller

  def sync(conn, params) do
    state = parse_sync_state(params)
    Phoenix.PubSub.broadcast(Burrito.PubSub, "order:sync", {:llv_sync, state})
    json(conn, %{ok: true})
  end

  defp parse_sync_state(params) do
    builder_params = params["builder"] || %{}
    cart_params = params["cart"] || []

    builder = %{
      base: builder_params["base"] || "white_rice",
      protein: builder_params["protein"] || "chicken",
      toppings: builder_params["toppings"] || [],
      extras: builder_params["extras"] || [],
      quantity: parse_int(builder_params["quantity"], 1),
      notes: builder_params["notes"] || ""
    }

    cart =
      Enum.map(cart_params, fn item ->
        %{
          id: item["id"] || :erlang.unique_integer([:positive]) |> to_string(),
          base: item["base"] || "white_rice",
          protein: item["protein"] || "chicken",
          toppings: item["toppings"] || [],
          extras: item["extras"] || [],
          quantity: parse_int(item["quantity"], 1),
          notes: item["notes"] || "",
          price: parse_float(item["price"], 9.50)
        }
      end)

    cart_total = Enum.reduce(cart, 0.0, fn item, acc -> acc + item.price * item.quantity end)

    %{builder: builder, cart: cart, cart_total: cart_total}
  end

  defp parse_int(nil, default), do: default
  defp parse_int(val, _default) when is_integer(val), do: val

  defp parse_int(val, default) when is_binary(val) do
    case Integer.parse(val) do
      {n, _} -> n
      :error -> default
    end
  end

  defp parse_float(nil, default), do: default
  defp parse_float(val, _default) when is_float(val), do: val
  defp parse_float(val, _default) when is_integer(val), do: val / 1

  defp parse_float(val, default) when is_binary(val) do
    case Float.parse(val) do
      {f, _} -> f
      :error -> default
    end
  end
end
