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
      <.input type="text" field={@form[:email]} />
      <div class="centered">
        <button class="ghost-button" disabled={@disabled}>SAVE</button>
      </div>
    </.form>
    <%= for error <- @errors do %>
      <p style="color:red;">{error}</p>
    <% end %>
    <div class="bordered">
      <h1>User List:</h1>
      <ul>
        <%= for user <- @users do %>
          <li>Username: {user["username"]}, Email: {user["email"]}</li>
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
    {:ok, assign(socket, users: users, form: to_form(user), errors: [], disabled: true)}
  end

  @impl true
  def handle_event("validate", params, socket) do
    errors = validate(params, socket.assigns.users)
    {:noreply, assign(socket, form: to_form(params), errors: errors, disabled: errors != [])}
  end

  def handle_event("save", user_params, socket) do
    users = socket.assigns.users
    IO.inspect "SEND"
    LocalLiveView.ServerSocket.send(user_params, __MODULE__)
    case validate(user_params, users) do
      [] ->
        user = %{"email" => "", "username" => ""}

        {:noreply,
         assign(socket,
           form: to_form(user),
           users: users ++ [user_params],
           errors: [],
           disabled: true
         )}

      errors ->
        {:noreply, assign(socket, errors: errors, disabled: true)}
    end
  end

  defp validate(user, existing_users) do
    (validate_correctness(user) ++ validate_already_existing(user, existing_users))
    |> Enum.filter(fn error -> error != "" end)
  end

  defp validate_already_existing(user, existing_users) do
    user
    |> Enum.filter(fn {key, value} ->
      Enum.any?(existing_users, fn user -> Map.get(user, key) == value end)
    end)
    |> Enum.map(fn {key, _value} ->
      String.capitalize("#{key} already in use")
    end)
  end

  defp validate_correctness(user) do
    Enum.map(user, fn {key, value} -> validate_correctness(key, value) end)
  end

  defp validate_correctness("username", value) do
    cond do
      String.length(value) < 4 -> "Username length must be greater than 3 characters"
      true -> ""
    end
  end

  defp validate_correctness("email", value) do
    with [name, server] <- String.split(value, "@"),
         true <- String.length(name) > 0 and String.contains?(server, ".")
      do
        ""
      else
        _err -> "Email must have an email format"
    end
  end
end
