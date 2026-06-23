defmodule Local.AddColumnComponent do
  use Phoenix.LiveComponent

  # Owns its own input/error state. On a valid submit it notifies the parent
  # with `{:add_column, name}` (name trimmed and guaranteed non-empty).

  @impl true
  def update(assigns, socket) do
    {:ok,
     socket
     |> Phoenix.Component.assign(assigns)
     |> Phoenix.Component.assign_new(:name, fn -> "" end)
     |> Phoenix.Component.assign_new(:error, fn -> nil end)}
  end

  @impl true
  def handle_event("change", %{"name" => name}, socket) do
    {:noreply, Phoenix.Component.assign(socket, name: name, error: nil)}
  end

  def handle_event("submit", %{"name" => name}, socket) do
    case String.trim(name) do
      "" ->
        {:noreply, Phoenix.Component.assign(socket, :error, "Column name cannot be empty")}

      name ->
        send(self(), {:add_column, name})
        {:noreply, Phoenix.Component.assign(socket, name: "", error: nil)}
    end
  end

  @impl true
  def render(assigns) do
    ~H"""
    <form
      phx-submit="submit"
      phx-change="change"
      phx-target={@myself}
      style="flex:0 0 280px;background:#111827;border:1px dashed #374151;border-radius:8px;padding:0.75em;display:flex;flex-direction:column;gap:0.5em"
    >
      <div style="font-weight:600;color:#9ca3af;font-size:0.95em">Add column</div>
      <input
        type="text"
        name="name"
        value={@name}
        placeholder="Column name..."
        autocomplete="off"
        style={"background:#1f2937;color:#f3f4f6;border:1px solid #{if @error, do: "#dc2626", else: "#374151"};border-radius:5px;padding:0.45em 0.55em;font-size:0.9em;outline:none"}
      />
      <span :if={@error} style="color:#fca5a5;font-size:0.8em">{@error}</span>
      <button
        type="submit"
        style="background:#16a34a;color:#fff;border:none;border-radius:5px;padding:0.45em 0.6em;font-size:0.9em;cursor:pointer"
      >Add column</button>
    </form>
    """
  end
end
