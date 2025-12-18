defmodule FormDemoLocal do
  use LocalLiveView
  import Local.CoreComponents

  @impl true
  def render(assigns) do
    ~H"""
    <div class="bordered">
      <.form for={@form} id="my-form" pop-change="validate" pop-submit="save">
        <label>USERNAME</label>
        <.input type="text" field={@form[:username]} />
        <label>EMAIL</label>
        <.input type="text" field={@form[:email]} />
        <div class="centered">
          <button class="ghost-button" disabled={@disabled}>SAVE</button>
        </div>
      </.form>
      <div class="centered">
        <button class="ghost-button" pop-click="generate_random">GENERATE RANDOM</button>
      </div>
    </div>
    <%= for error <- @errors do %>
      <p style="color:red;">{error}</p>
    <% end %>
    <div class="bordered">
      <h1>[Local Runtime] User List:</h1>
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
    send(self(), :sync)
    user = %{"email" => "", "username" => ""}
    {:ok, assign(socket, users: [], form: to_form(user), errors: [], disabled: true)}
  end

  @impl true
  def handle_event("validate", params, socket) do
    errors = validate(params, socket.assigns.users)
    {:noreply, assign(socket, form: to_form(params), errors: errors, disabled: errors != [])}
  end

  def handle_event("save", user_params, socket) do
    users = socket.assigns.users

    case validate(user_params, users) do
      [] ->
        blank_user = %{"email" => "", "username" => ""}
        send_to_phoenix(%{"type" => "new_user", "user" => user_params})

        {:noreply,
         assign(socket,
           form: to_form(blank_user),
           users: users ++ [user_params],
           errors: [],
           disabled: true
         )}

      errors ->
        {:noreply, assign(socket, errors: errors, disabled: true)}
    end
  end

  def handle_event(
        "llv_server_message",
        %{"type" => "synchronize", "users" => server_users},
        socket
      ) do
    filtered_users =
      Enum.filter(socket.assigns.users, fn user ->
        case validate_already_existing(user, server_users) do
          [] ->
            send_to_phoenix(%{"type" => "new_user", "user" => user})
            true

          _ ->
            false
        end
      end)

    {:noreply, assign(socket, users: server_users ++ filtered_users)}
  end

  def handle_event("generate_random", _params, socket) do
    users = socket.assigns.users
    user = generate_random_user(users)
    handle_event("save", user, socket)
  end

  def handle_info(:sync, socket) do
    send_to_phoenix(%{"type" => "sync_request"})
    {:noreply, socket}
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
         true <- String.length(name) > 0 and String.contains?(server, ".") do
      ""
    else
      _err -> "Email must have an email format"
    end
  end

  defp generate_random_user(existing_users) do
    number = to_string(Enum.random(1..999))
    user = %{"email" => "user#{number}@example.com", "username" => "user#{number}"}
    result = validate_already_existing(user, existing_users)

    case result do
      [] -> user
      _ -> generate_random_user(existing_users)
    end
  end

  defp send_to_phoenix(message) do
    LocalLiveView.ServerSocket.send(message, __MODULE__)
  end
end
