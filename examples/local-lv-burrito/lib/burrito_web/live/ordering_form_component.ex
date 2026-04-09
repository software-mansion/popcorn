defmodule BurritoWeb.Live.OrderingFormComponent do
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

  def mount(socket) do
    builder = default_builder()

    {:ok,
     assign(socket,
       builder: builder,
       builder_price: calculate_price(builder),
       cart: [],
       cart_total: 0.0
     )}
  end

  def handle_event("set_base", %{"base" => base}, socket) do
    builder = %{socket.assigns.builder | base: base}
    {:noreply, assign(socket, builder: builder, builder_price: calculate_price(builder))}
  end

  def handle_event("set_protein", %{"protein" => protein}, socket) do
    builder = %{socket.assigns.builder | protein: protein}
    {:noreply, assign(socket, builder: builder, builder_price: calculate_price(builder))}
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
    {:noreply, assign(socket, builder: builder, builder_price: calculate_price(builder))}
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
    {:noreply, assign(socket, builder: builder, builder_price: calculate_price(builder))}
  end

  def handle_event("inc_qty", _params, socket) do
    qty = min(socket.assigns.builder.quantity + 1, 10)
    builder = %{socket.assigns.builder | quantity: qty}
    {:noreply, assign(socket, builder: builder, builder_price: calculate_price(builder))}
  end

  def handle_event("dec_qty", _params, socket) do
    qty = max(socket.assigns.builder.quantity - 1, 1)
    builder = %{socket.assigns.builder | quantity: qty}
    {:noreply, assign(socket, builder: builder, builder_price: calculate_price(builder))}
  end

  def handle_event("add_to_cart", _params, socket) do
    builder = socket.assigns.builder
    price = calculate_price(builder)

    item =
      Map.merge(builder, %{
        id: "item_#{:erlang.unique_integer([:positive])}",
        price: price
      })

    cart = socket.assigns.cart ++ [item]
    cart_total = Enum.sum(Enum.map(cart, & &1.price))
    new_builder = default_builder()

    {:noreply,
     assign(socket,
       cart: cart,
       cart_total: cart_total,
       builder: new_builder,
       builder_price: calculate_price(new_builder)
     )}
  end

  def handle_event("remove_item", %{"id" => id}, socket) do
    cart = Enum.reject(socket.assigns.cart, &(&1.id == id))
    cart_total = Enum.sum(Enum.map(cart, & &1.price))
    {:noreply, assign(socket, cart: cart, cart_total: cart_total)}
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
    <div class="lv-order-builder p-4" phx-hook="LVRoundTripHook" id="lv-order-builder-form">
      <h3 class="text-base font-bold text-pop-brown my-4">Build Your Bowl</h3>

      <div class="section mb-3 rounded-xl overflow-hidden border border-pop-orange-light bg-white shadow-sm">
        <div class="px-3 py-2.5 bg-pop-cream-warm border-b border-pop-orange-light/60">
          <span class="text-sm font-semibold text-pop-brown-medium">Base</span>
        </div>
        <div class="px-3 pb-3 pt-2 space-y-2">
          <%= for {label, value} <- [{"White Rice", "white_rice"}, {"Brown Rice", "brown_rice"}, {"No Rice", "no_rice"}] do %>
            <label class="flex items-center gap-2 text-sm text-pop-brown cursor-pointer">
              <input
                type="radio"
                name={"lv_base_#{@id}"}
                value={value}
                checked={@builder.base == value}
                phx-click="set_base"
                phx-value-base={value}
                phx-target={@myself}
              />
              {label}
            </label>
          <% end %>
        </div>
      </div>

      <div class="section mb-3 rounded-xl overflow-hidden border border-pop-orange-light bg-white shadow-sm">
        <div class="px-3 py-2.5 bg-pop-cream-warm border-b border-pop-orange-light/60">
          <span class="text-sm font-semibold text-pop-brown-medium">Protein</span>
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
                name={"lv_protein_#{@id}"}
                value={value}
                checked={@builder.protein == value}
                phx-click="set_protein"
                phx-value-protein={value}
                phx-target={@myself}
              />
              {label}
              <span :if={price} class="text-xs text-pop-orange-dark font-semibold">{price}</span>
            </label>
          <% end %>
        </div>
      </div>

      <div class="section mb-3 rounded-xl overflow-hidden border border-pop-orange-light bg-white shadow-sm">
        <div class="px-3 py-2.5 bg-pop-cream-warm border-b border-pop-orange-light/60">
          <span class="text-sm font-semibold text-pop-brown-medium">Toppings</span>
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
                phx-target={@myself}
              />
              <span>{label}</span>
              <span :if={price} class="text-xs text-pop-orange-dark font-semibold">{price}</span>
            </label>
          <% end %>
        </div>
      </div>

      <div class="section mb-3 rounded-xl overflow-hidden border border-pop-orange-light bg-white shadow-sm">
        <div class="px-3 py-2.5 bg-pop-cream-warm border-b border-pop-orange-light/60">
          <span class="text-sm font-semibold text-pop-brown-medium">Extras</span>
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
                phx-target={@myself}
              />
              {label}
              <span class="text-xs text-pop-orange-dark font-semibold">{price}</span>
            </label>
          <% end %>
        </div>
      </div>

      <div class="section mb-3 rounded-xl overflow-hidden border border-pop-orange-light bg-white shadow-sm">
        <div class="px-3 py-2.5 bg-pop-cream-warm border-b border-pop-orange-light/60">
          <span class="text-sm font-semibold text-pop-brown-medium">Quantity</span>
        </div>
        <div class="px-3 pb-3 pt-2 flex items-center gap-3">
          <button
            phx-click="dec_qty"
            phx-target={@myself}
            class="w-8 h-8 rounded-full border-2 border-pop-brown-medium text-pop-brown-medium hover:bg-pop-orange-light font-bold flex items-center justify-center disabled:opacity-30 transition-colors"
            disabled={@builder.quantity <= 1}
          >
            −
          </button>
          <span class="text-xl font-bold text-pop-brown w-8 text-center">{@builder.quantity}</span>
          <button
            phx-click="inc_qty"
            phx-target={@myself}
            class="w-8 h-8 rounded-full border-2 border-pop-brown-medium text-pop-brown-medium hover:bg-pop-orange-light font-bold flex items-center justify-center disabled:opacity-30 transition-colors"
            disabled={@builder.quantity >= 10}
          >
            +
          </button>
        </div>
      </div>

      <div class="price-section bg-pop-orange-light rounded-xl px-4 py-3.5 mb-3 border border-pop-orange/20">
        <div class="flex items-center justify-between mb-3">
          <span class="text-sm text-pop-brown-medium font-medium">Item Total</span>
          <span class="text-2xl font-bold text-pop-brown">${format_price(@builder_price)}</span>
        </div>
        <button
          phx-click="add_to_cart"
          phx-target={@myself}
          phx-disable-with="Adding…"
          class="w-full bg-pop-brown hover:bg-pop-brown-medium text-white text-sm font-semibold py-2.5 px-4 rounded-lg transition-colors"
        >
          Add to Cart
        </button>
      </div>

      <div :if={@cart != []} class="cart-section border border-pop-orange-light rounded-xl bg-white p-3">
        <h4 class="text-sm font-semibold text-pop-brown-medium mb-2 flex items-center gap-1.5">
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
                  phx-target={@myself}
                  class="text-red-400 hover:text-red-600 font-bold text-base leading-none"
                  title="Remove"
                >
                  ×
                </button>
              </div>
            </div>
          <% end %>
        </div>
        <button class="mt-3 w-full bg-pop-brown hover:bg-pop-brown-medium text-white text-xs font-semibold py-2.5 px-4 rounded-lg transition-colors">
          Place Order
        </button>
      </div>

      <div :if={@cart == []} class="text-center py-6 text-pop-brown-medium/40 text-sm">
        Build your bowl and add to cart!
      </div>
    </div>
    """
  end
end
