defmodule FormDemoLocal do
  use LocalLiveView
  import Local.CoreComponents
  alias FormDemoLocal.User

  @impl true
  def render(assigns) do
    ~H"""
    <h2 class="header">Add new user</h2>
    <div class="bordered">
      <.form for={@form} id={@form.id} phx-change="validate" phx-submit="save">
        <label>USERNAME</label>
        <.input type="text" field={@form[:username]} placeholder="at least 4 characters" />
        <label>EMAIL</label>
        <.input type="text" field={@form[:email]} placeholder="name@domain.com" />
        <div class="buttons">
          <button type="submit" class="ghost-button" disabled={@disabled}>SAVE</button>
          <button type="button" class="ghost-button" phx-click="generate_random">
            GENERATE RANDOM
          </button>
        </div>
      </.form>
    </div>
    <div class="bordered" data-value="users-list">
      <h2 class="title">[Local Runtime] User List:</h2>
      <%= if Enum.empty?(@users) do %>
        <p>No users yet - save one above</p>
      <% end %>
      <ul>
        <%= for user <- @users do %>
          <li>Username: {user.username}, Email: {user.email}</li>
        <% end %>
      </ul>
    </div>
    """
  end

  @impl true
  def mount(_params, _session, socket) do
    {:ok, assign(socket, users: [], form: to_user_form(User.changeset(%User{}, %{}), []), disabled: true)}
  end

  @impl true
  def handle_event("validate", %{"user" => user_params}, socket) do
    users = socket.assigns.users
    changeset = User.changeset(%User{}, user_params, users)
    form = to_user_form(changeset, users, action: :validate)
    {:noreply, assign(socket, form: form, disabled: not changeset.valid?)}
  end

  def handle_event("save", %{"user" => user_params}, socket) do
    users = socket.assigns.users
    changeset = User.changeset(%User{}, user_params, users)

    case Ecto.Changeset.apply_action(changeset, :insert) do
      {:ok, user} ->
        users = users ++ [user]

        socket =
          assign(socket,
            users: users,
            form: to_user_form(User.changeset(%User{}, %{}), users),
            disabled: true
          )

        mirror_sync(socket, [:users])
        {:noreply, socket}

      {:error, changeset} ->
        form = to_user_form(changeset, users, action: :validate)
        {:noreply, assign(socket, form: form, disabled: true)}
    end
  end

  defp to_user_form(changeset, users, opts \\ []) do
    to_form(changeset, [id: "user-form-#{length(users)}"] ++ opts)
  end

  def handle_event("generate_random", _params, socket) do
    user_params = generate_random_user(socket.assigns.users)
    handle_event("save", %{"user" => user_params}, socket)
  end

  defp generate_random_user(existing_users) do
    number = to_string(Enum.random(1..999))
    user = %{"email" => "user#{number}@example.com", "username" => "user#{number}"}

    if Enum.any?(existing_users, fn existing -> existing.username == user["username"] end) do
      generate_random_user(existing_users)
    else
      user
    end
  end
end
