defmodule BurritoLive do
  use LocalLiveView

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

  def mount(_params, _session, socket) do
    builder = default_builder()

    {:ok,
     assign(socket,
       builder: builder,
       builder_price: calculate_price(builder),
       cart: [],
       cart_total: 0.0,
       next_id: 1
     )}
  end

  def handle_info(:sync, socket) do
    send_to_phoenix("sync_request", %{})
    {:noreply, socket}
  end

  defp default_builder do
    %{
      base: "white_rice",
      protein: "chicken",
      toppings: [],
      extras: [],
      quantity: 1,
      notes: ""
    }
  end

  def handle_server_event("synchronize", %{"state" => server_state}, socket) do
    builder_p = server_state["builder"] || %{}

    builder = %{
      base: builder_p["base"] || "white_rice",
      protein: builder_p["protein"] || "chicken",
      toppings: builder_p["toppings"] || [],
      extras: builder_p["extras"] || [],
      quantity: builder_p["quantity"] || 1,
      notes: builder_p["notes"] || ""
    }

    cart = parse_cart(server_state["cart"] || [])
    cart_total = parse_float(server_state["cart_total"], 0.0)
    builder_price = parse_float(server_state["builder_price"], calculate_price(builder))
    next_id = derive_next_id(cart)

    {:noreply,
     assign(socket,
       builder: builder,
       builder_price: builder_price,
       cart: cart,
       cart_total: cart_total,
       next_id: next_id
     )}
  end

  def handle_event("set_base", %{"base" => base}, socket) do
    builder = %{socket.assigns.builder | base: base}

    socket =
      assign(socket,
        builder: builder,
        builder_price: calculate_price(builder)
      )

    send_to_phoenix("state_update", serialize_state(socket.assigns))
    {:noreply, socket}
  end

  def handle_event("set_protein", %{"protein" => protein}, socket) do
    builder = %{socket.assigns.builder | protein: protein}

    socket =
      assign(socket,
        builder: builder,
        builder_price: calculate_price(builder)
      )

    send_to_phoenix("state_update", serialize_state(socket.assigns))
    {:noreply, socket}
  end

  def handle_event("toggle_topping", %{"topping" => topping}, socket) do
    toppings = socket.assigns.builder.toppings

    new_toppings =
      if topping in toppings do
        List.delete(toppings, topping)
      else
        [topping | toppings]
      end

    builder = %{socket.assigns.builder | toppings: new_toppings}

    socket =
      assign(socket,
        builder: builder,
        builder_price: calculate_price(builder)
      )

    send_to_phoenix("state_update", serialize_state(socket.assigns))
    {:noreply, socket}
  end

  def handle_event("toggle_extra", %{"extra" => extra}, socket) do
    extras = socket.assigns.builder.extras

    new_extras =
      if extra in extras do
        List.delete(extras, extra)
      else
        [extra | extras]
      end

    builder = %{socket.assigns.builder | extras: new_extras}

    socket =
      assign(socket,
        builder: builder,
        builder_price: calculate_price(builder)
      )

    send_to_phoenix("state_update", serialize_state(socket.assigns))
    {:noreply, socket}
  end

  def handle_event("inc_qty", _params, socket) do
    qty = min(socket.assigns.builder.quantity + 1, 10)
    builder = %{socket.assigns.builder | quantity: qty}

    socket =
      assign(socket,
        builder: builder,
        builder_price: calculate_price(builder)
      )

    send_to_phoenix("state_update", serialize_state(socket.assigns))
    {:noreply, socket}
  end

  def handle_event("dec_qty", _params, socket) do
    qty = max(socket.assigns.builder.quantity - 1, 1)
    builder = %{socket.assigns.builder | quantity: qty}

    socket =
      assign(socket,
        builder: builder,
        builder_price: calculate_price(builder)
      )

    send_to_phoenix("state_update", serialize_state(socket.assigns))
    {:noreply, socket}
  end

  def handle_event("add_to_cart", _params, socket) do
    builder = socket.assigns.builder
    price = calculate_price(builder)
    item_id = "llv_item_#{socket.assigns.next_id}"

    item = Map.merge(builder, %{id: item_id, price: price})

    cart = socket.assigns.cart ++ [item]
    cart_total = Enum.sum(Enum.map(cart, & &1.price))
    new_builder = default_builder()

    socket =
      assign(socket,
        cart: cart,
        cart_total: cart_total,
        builder: new_builder,
        builder_price: calculate_price(new_builder),
        next_id: socket.assigns.next_id + 1
      )

    send_to_phoenix("state_update", serialize_state(socket.assigns))
    {:noreply, socket}
  end

  def handle_event("remove_item", %{"id" => id}, socket) do
    cart = Enum.reject(socket.assigns.cart, &(&1.id == id))
    cart_total = Enum.sum(Enum.map(cart, & &1.price))
    socket = assign(socket, cart: cart, cart_total: cart_total)
    send_to_phoenix("state_update", serialize_state(socket.assigns))
    {:noreply, socket}
  end

  def handle_server_event(_event, _params, socket) do
    {:noreply, socket}
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
        notes: item["notes"] || "",
        price: parse_float(item["price"], 0.0)
      }
    end)
  end

  defp parse_float(val, _default) when is_float(val), do: val
  defp parse_float(val, _default) when is_integer(val), do: val * 1.0
  defp parse_float(_, default), do: default

  defp derive_next_id([]), do: 1

  defp derive_next_id(cart) do
    max_id =
      cart
      |> Enum.map(fn item ->
        case Integer.parse(String.replace(item.id, "llv_item_", "")) do
          {n, ""} -> n
          _ -> 0
        end
      end)
      |> Enum.max()

    max_id + 1
  end

  defp send_to_phoenix(event, payload) do
    LocalLiveView.ServerSocket.send(event, payload, __MODULE__)
  end

  defp serialize_state(assigns) do
    b = assigns.builder

    %{
      "builder" => %{
        "base" => b.base,
        "protein" => b.protein,
        "toppings" => b.toppings,
        "extras" => b.extras,
        "quantity" => b.quantity,
        "notes" => b.notes
      },
      "builder_price" => assigns.builder_price,
      "cart" =>
        Enum.map(assigns.cart, fn item ->
          %{
            "id" => item.id,
            "base" => item.base,
            "protein" => item.protein,
            "toppings" => item.toppings,
            "extras" => item.extras,
            "quantity" => item.quantity,
            "notes" => item.notes,
            "price" => item.price
          }
        end),
      "cart_total" => assigns.cart_total
    }
  end

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
    cents = round(price * 100)
    dollars = div(cents, 100)
    c = rem(cents, 100)
    if c < 10, do: "#{dollars}.0#{c}", else: "#{dollars}.#{c}"
  end

  def render(assigns) do
    ~H"""
    <div
      id="llv-inner"
      class="llv-order-builder p-4"
      data-cart-count={length(@cart)}
      data-cart-total={format_price(@cart_total)}
    >
      <h3 class="text-base font-bold text-pop-brown my-4">Build Your Bowl</h3>

      <div class="section mb-3 rounded-xl overflow-hidden border border-pop-orange-light bg-white shadow-sm">
        <div class="px-3 py-2.5 bg-pop-orange-light/30 border-b border-pop-orange-light/60">
          <span class="text-sm font-semibold text-pop-orange-dark">Base</span>
        </div>
        <div class="px-3 pb-3 pt-2 space-y-2">
          <%= for {label, value} <- [{"White Rice", "white_rice"}, {"Brown Rice", "brown_rice"}, {"No Rice", "no_rice"}] do %>
            <label class="flex items-center gap-2 text-sm text-pop-brown cursor-pointer">
              <input
                type="radio"
                name="llv_base"
                value={value}
                checked={@builder.base == value}
                phx-click="set_base"
                phx-value-base={value}
              />
              {label}
            </label>
          <% end %>
        </div>
      </div>

      <div class="section mb-3 rounded-xl overflow-hidden border border-pop-orange-light bg-white shadow-sm">
        <div class="px-3 py-2.5 bg-pop-orange-light/30 border-b border-pop-orange-light/60">
          <span class="text-sm font-semibold text-pop-orange-dark">Protein</span>
        </div>
        <div class="px-3 pb-3 pt-2 space-y-2">
          <%= for {label, value, price} <- [
            {"Chicken", "chicken", nil},
            {"Steak", "steak", "+$2.00"},
            {"No Protein", "no_protein", nil}
          ] do %>
            <label class="flex items-center gap-2 text-sm text-pop-brown cursor-pointer">
              <input
                type="radio"
                name="llv_protein"
                value={value}
                checked={@builder.protein == value}
                phx-click="set_protein"
                phx-value-protein={value}
              />
              {label}
              <span :if={price} class="text-xs text-pop-orange-dark font-semibold">{price}</span>
            </label>
          <% end %>
        </div>
      </div>

      <div class="section mb-3 rounded-xl overflow-hidden border border-pop-orange-light bg-white shadow-sm">
        <div class="px-3 py-2.5 bg-pop-orange-light/30 border-b border-pop-orange-light/60">
          <span class="text-sm font-semibold text-pop-orange-dark">Toppings</span>
        </div>
        <div class="px-3 pb-3 pt-2 grid grid-cols-2 gap-1.5">
          <%= for {label, value, price} <- [
            {"Black Beans", "black_beans", nil},
            {"Tomato Salsa", "tomato_salsa", nil},
            {"Sour Cream", "sour_cream", nil},
            {"Cheese", "cheese", nil},
            {"Guacamole", "guacamole", "+$2.50"}
          ] do %>
            <label class="flex items-center gap-1.5 text-sm text-pop-brown cursor-pointer">
              <input
                type="checkbox"
                checked={value in @builder.toppings}
                phx-click="toggle_topping"
                phx-value-topping={value}
              />
              <span>{label}</span>
              <span :if={price} class="text-xs text-pop-orange-dark font-semibold">{price}</span>
            </label>
          <% end %>
        </div>
      </div>

      <div class="section mb-3 rounded-xl overflow-hidden border border-pop-orange-light bg-white shadow-sm">
        <div class="px-3 py-2.5 bg-pop-orange-light/30 border-b border-pop-orange-light/60">
          <span class="text-sm font-semibold text-pop-orange-dark">Extras</span>
        </div>
        <div class="px-3 pb-3 pt-2 space-y-2">
          <%= for {label, value, price} <- [
            {"Extra Protein", "extra_protein", "+$3.50"},
            {"Chips", "chips", "+$1.50"},
            {"Large Drink", "large_drink", "+$2.50"}
          ] do %>
            <label class="flex items-center gap-2 text-sm text-pop-brown cursor-pointer">
              <input
                type="checkbox"
                checked={value in @builder.extras}
                phx-click="toggle_extra"
                phx-value-extra={value}
              />
              {label}
              <span class="text-xs text-pop-orange-dark font-semibold">{price}</span>
            </label>
          <% end %>
        </div>
      </div>

      <div class="section mb-3 rounded-xl overflow-hidden border border-pop-orange-light bg-white shadow-sm">
        <div class="px-3 py-2.5 bg-pop-orange-light/30 border-b border-pop-orange-light/60">
          <span class="text-sm font-semibold text-pop-orange-dark">Quantity</span>
        </div>
        <div class="px-3 pb-3 pt-2 flex items-center gap-3">
          <button
            phx-click="dec_qty"
            class="w-8 h-8 rounded-full border-2 border-pop-orange text-pop-orange-dark hover:bg-pop-orange-light font-bold flex items-center justify-center transition-colors"
          >
            −
          </button>
          <span class="text-xl font-bold text-pop-brown w-8 text-center">{@builder.quantity}</span>
          <button
            phx-click="inc_qty"
            class="w-8 h-8 rounded-full border-2 border-pop-orange text-pop-orange-dark hover:bg-pop-orange-light font-bold flex items-center justify-center transition-colors"
          >
            +
          </button>
        </div>
      </div>

      <div class="price-section bg-pop-orange rounded-xl px-4 py-3.5 mb-3">
        <div class="flex items-center justify-between mb-3">
          <span class="text-sm text-white/80 font-medium">Item Total</span>
          <span class="text-2xl font-bold text-white">${format_price(@builder_price)}</span>
        </div>
        <button
          phx-click="add_to_cart"
          class="w-full bg-white text-pop-orange-dark hover:bg-pop-orange-light text-sm font-semibold py-2.5 px-4 rounded-lg transition-colors"
        >
          Add to Cart
        </button>
      </div>

      <div :if={@cart != []} class="cart-section border border-pop-orange/30 rounded-xl bg-white p-3">
        <h4 class="text-sm font-semibold text-pop-orange-dark mb-2 flex items-center gap-1.5">
          <span class="hero-shopping-cart size-4"></span>
          Cart ({length(@cart)} items) — ${format_price(@cart_total)}
        </h4>
        <div class="space-y-2">
          <%= for item <- @cart do %>
            <div class="flex items-start justify-between text-xs text-pop-brown border-b border-pop-orange-light pb-2 last:border-0 last:pb-0">
              <div>
                <span class="font-semibold">Burrito Bowl ×{item.quantity}</span>
                <div class="text-pop-brown-medium/60 mt-0.5">
                  {item.base |> String.replace("_", " ")} · {item.protein |> String.replace("_", " ")}
                </div>
              </div>
              <div class="flex items-center gap-2 flex-shrink-0 ml-2">
                <span class="font-semibold">${format_price(item.price)}</span>
                <button
                  phx-click="remove_item"
                  phx-value-id={item.id}
                  class="text-red-400 hover:text-red-600 font-bold text-base leading-none"
                  title="Remove"
                >
                  ×
                </button>
              </div>
            </div>
          <% end %>
        </div>
        <button class="mt-3 w-full bg-pop-orange-dark hover:bg-pop-orange text-white text-xs font-semibold py-2.5 px-4 rounded-lg transition-colors">
          Place Order
        </button>
      </div>

      <div :if={@cart == []} class="text-center py-6 text-pop-brown-medium text-sm">
        Build your bowl and add to cart!
      </div>
    </div>
    """
  end
end
