defmodule FormDemoLocal do
  use LocalLiveView
  import Local.CoreComponents

  @impl true
  def render(assigns) do
    ~H"""
    <.form for={@form} id="my-form" pop-change="validate" pop-submit="save" class="bordered">
      <label>USERNAME</label>
      <.input type="text" field={@form[:username]} />
      <label>EMAIL</label>
      <.input type="email" field={@form[:email]} />
      <div class="centered">
        <button class="ghost-button" disabled={@disabled}>SAVE</button>
      </div>
    </.form>
    <p style="color:red;"><%= @errors %></p>
    <div class="bordered">
      <h1>User List:</h1>
      <ul>
        <%= for user <- @users do %>
          <li>Username: <%= user["username"] %>, Email: <%= user["email"] %></li>
        <% end %>
      </ul>
    </div>
    """
  end

  @impl true
  def mount(_params, _session, socket) do
    users = [
      %{"email" => "user1@example.com", "username" => "user1"},
      %{"email" => "user2@example.com", "username" => "user2"},
      %{"email" => "user3@example.com", "username" => "user3"}
    ]
    user = %{"email" => "", "username" => ""}
    {:ok, assign(socket, users: users, form: to_form(user), errors: "", disabled: false)}
  end

  @impl true
  def handle_event("validate", params, socket) do
    errors = validate(params, socket.assigns.users)
    {:noreply, assign(socket, form: to_form(params), errors: errors, disabled: errors != "")}
  end

  def handle_event("save", user_params, socket) do
    users = socket.assigns.users
    case validate(user_params, users) do
      "" ->
        {:noreply, assign(socket, users: users ++ [user_params], errors: "")}

      errors ->
        {:noreply, assign(socket, errors: errors)}
    end
  end

  defp validate(user, existing_users) do
    user
    |> Enum.filter(fn {key, value} ->
      Enum.any?(existing_users, fn user -> Map.get(user, key) == value end)
    end)
    |> Enum.map_join(", ", fn {key, _value} ->
      String.capitalize("#{key} already in use")
      end)
  end
end
