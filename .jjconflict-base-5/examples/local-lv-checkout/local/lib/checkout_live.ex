defmodule CheckoutLive do
  use LocalLiveView

  def mount(params, _session, socket) do
    {:ok,
     assign(socket,
       current_step: 1,
       form_data: %{
         "email" => "",
         "name" => "",
         "card_number" => "",
         "expiry" => ""
       },
       errors: []
     )}
  end

  def handle_params(%{"step" => step_str}, _uri, socket) do
    step = String.to_integer(step_str)
    {:noreply, assign(socket, current_step: step, errors: [])}
  end

  def handle_params(params, _uri, socket) do
    {:noreply, assign(socket, current_step: 1)}
  end

  def render(assigns) do
    ~H"""
    <div style="padding: 32px;">
      <%= case @current_step do %>
        <% 1 -> %>
          <.step_1_shipping form_data={@form_data} errors={@errors} />
        <% 2 -> %>
          <.step_2_payment form_data={@form_data} errors={@errors} />
        <% 3 -> %>
          <.step_3_confirmation form_data={@form_data} />
        <% _ -> %>
          <.step_4_success />
      <% end %>

      <%= if @current_step < 4 do %>
        <div style="display: flex; gap: 16px; margin-top: 32px; justify-content: space-between;">
          <%= if @current_step > 1 do %>
            <button
              phx-click="prev_step"
              style="padding: 8px 24px; background-color: #d1d5db; color: #1f2937; border-radius: 4px; border: none; cursor: pointer; font-weight: 500;"
            >
              ← Back
            </button>
          <% else %>
            <div></div>
          <% end %>

          <%= if @current_step < 3 do %>
            <button
              phx-click="next_step"
              disabled={@errors != []}
              style={button_style(@errors != [])}
            >
              Next →
            </button>
          <% end %>

          <%= if @current_step == 3 do %>
            <button
              phx-click="complete"
              style="padding: 8px 24px; background-color: #22c55e; color: white; border-radius: 4px; border: none; cursor: pointer; font-weight: 500;"
            >
              Complete Order
            </button>
          <% end %>
        </div>
      <% end %>
    </div>
    """
  end

  defp button_style(disabled) do
    if disabled do
      "padding: 8px 24px; background-color: #9ca3af; color: white; border-radius: 4px; border: none; cursor: not-allowed; font-weight: 500;"
    else
      "padding: 8px 24px; background-color: #3b82f6; color: white; border-radius: 4px; border: none; cursor: pointer; font-weight: 500;"
    end
  end

  def handle_event("update_field", params, socket) do
    # Extract the field that was targeted from the form
    [field_name] = params["_target"]
    value = params[field_name]

    form_data = Map.put(socket.assigns.form_data, field_name, value)
    errors = validate_step(socket.assigns.current_step, form_data)

    {:noreply, assign(socket, form_data: form_data, errors: errors)}
  end

  def handle_event("next_step", _params, socket) do
    step = socket.assigns.current_step
    errors = validate_step(step, socket.assigns.form_data)

    if errors == [] do
      {:noreply, push_patch(socket, to: "/?step=#{step + 1}")}
    else
      {:noreply, assign(socket, errors: errors)}
    end
  end

  def handle_event("prev_step", _params, socket) do
    {:noreply, push_patch(socket, to: "/?step=#{socket.assigns.current_step - 1}")}
  end

  def handle_event("complete", _params, socket) do
    data = socket.assigns.form_data
    IO.inspect(data, label: "Order submitted")
    {:noreply, assign(socket, current_step: 4)}
  end

  defp validate_step(1, form_data) do
    errors = []

    errors =
      if String.trim(form_data["email"]) == "", do: ["Email is required" | errors], else: errors

    errors =
      if String.trim(form_data["name"]) == "", do: ["Name is required" | errors], else: errors

    Enum.reverse(errors)
  end

  defp validate_step(2, form_data) do
    errors = []

    errors =
      if String.trim(form_data["card_number"]) == "",
        do: ["Card number is required" | errors],
        else: errors

    errors =
      if String.trim(form_data["expiry"]) == "",
        do: ["Expiry date is required" | errors],
        else: errors

    Enum.reverse(errors)
  end

  defp validate_step(3, _form_data), do: []
  defp validate_step(_, _form_data), do: []

  defp step_1_shipping(assigns) do
    ~H"""
    <div>
      <h2 style="font-size: 24px; font-weight: bold; margin-bottom: 24px; color: #1f2937;">
        Shipping Information
      </h2>
      <form style="display: flex; flex-direction: column; gap: 16px;">
        <div>
          <label style="display: block; font-size: 14px; font-weight: 500; color: #374151; margin-bottom: 8px;">Email</label>
          <input
            type="email"
            name="email"
            value={@form_data["email"]}
            phx-change="update_field"
            style="width: 100%; padding: 8px 16px; border: 1px solid #d1d5db; border-radius: 4px; font-size: 14px; box-sizing: border-box; color: black; color: black;"
            placeholder="you@example.com"
          />
        </div>

        <div>
          <label style="display: block; font-size: 14px; font-weight: 500; color: #374151; margin-bottom: 8px;">Full Name</label>
          <input
            type="text"
            name="name"
            value={@form_data["name"]}
            phx-change="update_field"
            style="width: 100%; padding: 8px 16px; border: 1px solid #d1d5db; border-radius: 4px; font-size: 14px; box-sizing: border-box; color: black;"
            placeholder="John Doe"
          />
        </div>

        <%= if @errors != [] do %>
          <div style="background-color: #fef2f2; border: 1px solid #fecaca; border-radius: 4px; padding: 16px;">
            <p style="color: #991b1b; font-weight: 600; margin-bottom: 8px; margin-top: 0;">
              Please fix the following:
            </p>
            <ul style="list-style: disc; margin-left: 20px; color: #b91c1c; margin-top: 0; margin-bottom: 0;">
              <%= for error <- @errors do %>
                <li>{error}</li>
              <% end %>
            </ul>
          </div>
        <% end %>
      </form>
    </div>
    """
  end

  defp step_2_payment(assigns) do
    ~H"""
    <div>
      <h2 style="font-size: 24px; font-weight: bold; margin-bottom: 24px; color: #1f2937;">
        Payment Information
      </h2>
      <form style="display: flex; flex-direction: column; gap: 16px;">
        <div>
          <label style="display: block; font-size: 14px; font-weight: 500; color: #374151; margin-bottom: 8px;">Card Number</label>
          <input
            type="text"
            name="card_number"
            value={@form_data["card_number"]}
            phx-change="update_field"
            style="width: 100%; padding: 8px 16px; border: 1px solid #d1d5db; border-radius: 4px; font-size: 14px; box-sizing: border-box; color: black;"
            placeholder="1234 5678 9012 3456"
            maxlength="19"
          />
        </div>

        <div>
          <label style="display: block; font-size: 14px; font-weight: 500; color: #374151; margin-bottom: 8px;">Expiry Date (MM/YY)</label>
          <input
            type="text"
            name="expiry"
            value={@form_data["expiry"]}
            phx-change="update_field"
            style="width: 100%; padding: 8px 16px; border: 1px solid #d1d5db; border-radius: 4px; font-size: 14px; box-sizing: border-box; color: black;"
            placeholder="12/25"
            maxlength="5"
          />
        </div>

        <%= if @errors != [] do %>
          <div style="background-color: #fef2f2; border: 1px solid #fecaca; border-radius: 4px; padding: 16px;">
            <p style="color: #991b1b; font-weight: 600; margin-bottom: 8px; margin-top: 0;">
              Please fix the following:
            </p>
            <ul style="list-style: disc; margin-left: 20px; color: #b91c1c; margin-top: 0; margin-bottom: 0;">
              <%= for error <- @errors do %>
                <li>{error}</li>
              <% end %>
            </ul>
          </div>
        <% end %>
      </form>
    </div>
    """
  end

  defp step_3_confirmation(assigns) do
    ~H"""
    <div>
      <h2 style="font-size: 24px; font-weight: bold; margin-bottom: 24px; color: #1f2937;">
        Order Summary
      </h2>
      <div style="background-color: #f3f4f6; border-radius: 4px; padding: 24px; display: flex; flex-direction: column; gap: 12px;">
        <div style="display: flex; justify-content: space-between; padding: 8px 0; border-bottom: 1px solid #d1d5db;">
          <span style="font-weight: 500;">Email:</span>
          <span>{@form_data["email"]}</span>
        </div>
        <div style="display: flex; justify-content: space-between; padding: 8px 0; border-bottom: 1px solid #d1d5db;">
          <span style="font-weight: 500;">Name:</span>
          <span>{@form_data["name"]}</span>
        </div>
        <div style="display: flex; justify-content: space-between; padding: 8px 0;">
          <span style="font-weight: 500;">Card ending in:</span>
          <span>••••{String.slice(@form_data["card_number"], -4..-1)}</span>
        </div>
      </div>

      <div style="margin-top: 24px; padding: 16px; background-color: #eff6ff; border: 1px solid #bfdbfe; border-radius: 4px;">
        <p style="color: #1e40af; margin: 0;">
          ✓ All information looks correct. Click "Complete Order" to finish.
        </p>
      </div>
    </div>
    """
  end

  defp step_4_success(assigns) do
    ~H"""
    <div style="text-align: center; padding: 32px 0;">
      <div style="width: 64px; height: 64px; background-color: #dcfce7; border-radius: 50%; display: flex; align-items: center; justify-content: center; margin: 0 auto 16px;">
        <span style="font-size: 32px;">✓</span>
      </div>
      <h2 style="font-size: 24px; font-weight: bold; color: #15803d; margin-bottom: 8px;">
        Order Complete!
      </h2>
      <p style="color: #6b7280; font-size: 14px;">Your order has been placed successfully.</p>
    </div>
    """
  end
end
