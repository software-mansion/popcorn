defmodule ImmortalGridWeb.CoreComponents do
  @moduledoc false
  use Phoenix.Component

  @doc """
  Renders the mount point for a Local LiveView (WASM).
  """
  attr :view, :string, required: true
  attr :id, :string

  def local_live_view(assigns) do
    assigns = assign_new(assigns, :id, fn -> assigns.view end)
    assigns = assign(assigns, :has_mirror, mirror_exists?(assigns.view))

    ~H"""
    <div
      data-pop-view={@view}
      id={@id}
      data-pop-mirror={@has_mirror || nil}
      phx-update="ignore"
    >
    </div>
    """
  end

  defp mirror_exists?(view_name) do
    mirror = Module.concat(Mirror, String.to_atom(view_name))
    Code.ensure_loaded?(mirror) and function_exported?(mirror, :handle_sync, 2)
  end

  @doc """
  Renders a status dot with a label.
  """
  attr :status, :atom, values: [:connected, :offline, :syncing]

  def status_indicator(%{status: :connected} = assigns) do
    ~H"""
    <div class="flex items-center gap-2">
      <div class="w-2.5 h-2.5 rounded-full bg-green-500 shadow-[0_0_6px_#22c55e]"></div>
      <span class="text-xs font-medium text-[#5f4122]">connected</span>
    </div>
    """
  end

  def status_indicator(%{status: :offline} = assigns) do
    ~H"""
    <div class="flex items-center gap-2">
      <div class="w-2.5 h-2.5 rounded-full bg-[#cf3f3f] shadow-[0_0_6px_#cf3f3f]"></div>
      <span class="text-xs font-medium text-[#cf3f3f]">server offline</span>
    </div>
    """
  end

  def status_indicator(%{status: :syncing} = assigns) do
    ~H"""
    <div class="flex items-center gap-2">
      <div class="w-2.5 h-2.5 rounded-full bg-[#d48f00] shadow-[0_0_6px_#d48f00] animate-pulse"></div>
      <span class="text-xs font-medium text-[#d48f00]">syncing…</span>
    </div>
    """
  end
end
