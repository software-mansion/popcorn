defmodule FormDemoLocal do
  use LocalLiveView
  import Local.CoreComponents

  def render(assigns) do
    ~H"""
    <.form for={@form} id="my-form" pop-change="validate" pop-submit="save" class="bordered">
      <label>USERNAME</label>
      <.input type="text" field={@form[:username]} />
      <label>EMAIL</label>
      <.input type="email" field={@form[:email]} />
      <div class="centered">
        <button class="ghost-button">SAVE</button>
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

  def mount(_params, _session, socket) do
    users = [
      %{"email" => "user1@example.com", "username" => "user1"},
      %{"email" => "user2@example.com", "username" => "user2"},
      %{"email" => "user3@example.com", "username" => "user3"}
    ]
    user = %{"email" => "", "username" => ""}
    {:ok, assign(socket, users: users, form: to_form(user), errors: "")}
  end

  def handle_event("validate", params, socket) do
    errors = validate(params, socket.assigns.users)
    {:noreply, assign(socket, form: to_form(params), errors: errors)}
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
    result =
      user
      |> Enum.reduce(%{}, fn {k, v}, acc ->
        in_existing_users = Enum.any?(existing_users, fn u -> Map.get(u, k) == v end)
        Map.put(acc, k, in_existing_users)
      end)

    Enum.reduce(result, "", fn
      {k, true}, acc -> acc <> "#{to_string(k)} already in use! "
      {_k, false}, acc -> acc
    end)
  end
end
