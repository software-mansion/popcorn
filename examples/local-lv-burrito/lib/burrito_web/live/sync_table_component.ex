defmodule BurritoWeb.Live.SyncTableComponent do
  use BurritoWeb, :live_component

  @base_price 9.50
  @protein_prices %{
    "chicken" => 0.0,
    "steak" => 2.0,
    "no_protein" => 0.0
  }
  @extra_prices %{
    "extra_protein" => 3.50,
    "chips" => 1.50,
    "large_drink" => 2.50
  }
  @topping_prices %{
    "guacamole" => 2.50
  }

  def update(%{sync_params: params} = _assigns, socket) do
    builder_p = params["builder"] || %{}

    builder = %{
      base: builder_p["base"] || "white_rice",
      protein: builder_p["protein"] || "chicken",
      toppings: builder_p["toppings"] || [],
      extras: builder_p["extras"] || [],
      quantity: builder_p["quantity"] || 1
    }

    cart = parse_cart(params["cart"] || [])
    cart_total = Enum.sum(Enum.map(cart, & &1.price))
    builder_price = parse_float(params["builder_price"], 0.0)

    {:ok,
     assign(socket,
       builder: builder,
       builder_price: builder_price,
       cart: cart,
       cart_total: cart_total
     )}
  end

  def update(assigns, socket) do
    default_builder = default_builder()

    socket =
      socket
      |> assign_new(:builder, fn -> default_builder end)
      |> assign_new(:builder_price, fn -> calculate_price(default_builder) end)
      |> assign_new(:cart, fn -> [] end)
      |> assign_new(:cart_total, fn -> 0.0 end)
      |> assign(:id, assigns.id)

    {:ok, socket}
  end

  defp default_builder do
    %{
      base: "white_rice",
      protein: "chicken",
      toppings: [],
      extras: [],
      quantity: 1
    }
  end

  defp parse_cart(items) when is_list(items) do
    Enum.map(items, fn item ->
      %{
        id: item["id"] || "item_unknown",
        base: item["base"] || "white_rice",
        protein: item["protein"] || "chicken",
        toppings: item["toppings"] || [],
        extras: item["extras"] || [],
        quantity: item["quantity"] || 1,
        price: parse_float(item["price"], 0.0)
      }
    end)
  end

  defp parse_float(val, _default) when is_float(val), do: val
  defp parse_float(val, _default) when is_integer(val), do: val * 1.0
  defp parse_float(_, default), do: default

  defp calculate_price(builder) do
    protein_add = Map.get(@protein_prices, builder.protein, 0.0)

    extras_add =
      builder.extras
      |> Enum.map(&Map.get(@extra_prices, &1, 0.0))
      |> Enum.sum()

    toppings_add =
      builder.toppings
      |> Enum.map(&Map.get(@topping_prices, &1, 0.0))
      |> Enum.sum()

    (@base_price + protein_add + extras_add + toppings_add) * builder.quantity
  end

  defp format_price(price) do
    :erlang.float_to_binary(price * 1.0, decimals: 2)
  end

  def render(assigns) do
    ~H"""
    <div class="p-4 bg-pop-cream">
      <div class="mb-4 border border-pop-orange-light rounded-lg overflow-hidden">
        <table class="w-full text-xs">
          <thead>
            <tr class="bg-pop-orange-light/40 border-b border-pop-orange-light">
              <th class="text-left px-3 py-2 font-semibold text-pop-brown-medium w-1/3">Field</th>
              <th class="text-left px-3 py-2 font-semibold text-pop-brown-medium">Value</th>
            </tr>
          </thead>
          <tbody class="divide-y divide-pop-orange-light/40">
            <tr class="hover:bg-pop-orange-light/10">
              <td class="px-3 py-2 text-pop-brown-medium/60">Base</td>
              <td class="px-3 py-2 text-pop-brown font-medium">
                {String.replace(@builder.base, "_", " ") |> String.capitalize()}
              </td>
            </tr>
            <tr class="hover:bg-pop-orange-light/10">
              <td class="px-3 py-2 text-pop-brown-medium/60">Protein</td>
              <td class="px-3 py-2 text-pop-brown font-medium">
                {String.replace(@builder.protein, "_", " ") |> String.capitalize()}
              </td>
            </tr>
            <tr class="hover:bg-pop-orange-light/10">
              <td class="px-3 py-2 text-pop-brown-medium/60">Toppings</td>
              <td class="px-3 py-2 text-pop-brown">
                <%= if @builder.toppings == [] do %>
                  <span class="text-pop-brown-medium/40 italic">none</span>
                <% else %>
                  {Enum.map_join(@builder.toppings, ", ", &String.replace(&1, "_", " "))}
                <% end %>
              </td>
            </tr>
            <tr class="hover:bg-pop-orange-light/10">
              <td class="px-3 py-2 text-pop-brown-medium/60">Extras</td>
              <td class="px-3 py-2 text-pop-brown">
                <%= if @builder.extras == [] do %>
                  <span class="text-pop-brown-medium/40 italic">none</span>
                <% else %>
                  {Enum.map_join(@builder.extras, ", ", &String.replace(&1, "_", " "))}
                <% end %>
              </td>
            </tr>
            <tr class="hover:bg-pop-orange-light/10">
              <td class="px-3 py-2 text-pop-brown-medium/60">Quantity</td>
              <td class="px-3 py-2 text-pop-brown font-medium">{@builder.quantity}</td>
            </tr>
            <tr class="bg-pop-orange-light/30">
              <td class="px-3 py-2 text-pop-orange-dark font-semibold">Item Price</td>
              <td class="px-3 py-2 text-pop-brown font-bold">${format_price(@builder_price)}</td>
            </tr>
          </tbody>
        </table>
      </div>

      <h4 class="text-xs font-semibold text-pop-brown-medium/60 uppercase tracking-wide mb-2 flex items-center gap-1">
        <.icon name="hero-shopping-cart" class="size-4" /> Cart ({length(@cart)} items)
      </h4>
      <div :if={@cart != []} class="border border-pop-orange-light rounded-lg overflow-hidden mb-2">
        <table class="w-full text-xs">
          <thead>
            <tr class="bg-pop-orange-light/40 border-b border-pop-orange-light">
              <th class="text-left px-3 py-2 font-semibold text-pop-brown-medium">#</th>
              <th class="text-left px-3 py-2 font-semibold text-pop-brown-medium">Base</th>
              <th class="text-left px-3 py-2 font-semibold text-pop-brown-medium">Protein</th>
              <th class="text-left px-3 py-2 font-semibold text-pop-brown-medium">Qty</th>
              <th class="text-right px-3 py-2 font-semibold text-pop-brown-medium">Price</th>
            </tr>
          </thead>
          <tbody class="divide-y divide-pop-orange-light/40">
            <%= for {item, idx} <- Enum.with_index(@cart, 1) do %>
              <tr class="hover:bg-pop-orange-light/10">
                <td class="px-3 py-2 text-pop-brown-medium/40">{idx}</td>
                <td class="px-3 py-2 text-pop-brown">{String.replace(item.base, "_", " ")}</td>
                <td class="px-3 py-2 text-pop-brown">{String.replace(item.protein, "_", " ")}</td>
                <td class="px-3 py-2 text-pop-brown">{item.quantity}</td>
                <td class="px-3 py-2 text-pop-brown font-semibold text-right">
                  ${format_price(item.price)}
                </td>
              </tr>
            <% end %>
            <tr class="bg-pop-orange-light/30 border-t border-pop-orange-light">
              <td colspan="4" class="px-3 py-2 text-pop-orange-dark font-semibold text-right">
                Total
              </td>
              <td class="px-3 py-2 text-pop-brown font-bold text-right">
                ${format_price(@cart_total)}
              </td>
            </tr>
          </tbody>
        </table>
      </div>
      <div
        :if={@cart == []}
        class="text-center py-4 text-pop-brown-medium/40 text-xs border border-dashed border-pop-orange-light/60 rounded-lg"
      >
        No items in cart yet
      </div>
    </div>
    """
  end
end
