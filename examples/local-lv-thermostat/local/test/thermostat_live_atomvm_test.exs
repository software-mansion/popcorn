defmodule ThermostatLiveAtomVMTest do
  @moduledoc """
  Runs the offline (LocalLiveView) thermostat on a real native AtomVM, the same runtime
  that powers the example in the browser. We compile a quoted expression that mounts
  the view, handles the increment/decrement events and renders each state to HTML, run
  it on the AtomVM binary, and assert on the HTML it produces.

  Build the runtime once with:

      MIX_TARGET=wasm mix popcorn.build_runtime --target unix --out-dir test/popcorn_runtime_source
  """
  use ExUnit.Case
  alias Popcorn.Support.AtomVM

  @moduletag :tmp_dir
  @moduletag timeout: :timer.minutes(3)

  test "ThermostatLive mount/handle_event/render runs on AtomVM", %{tmp_dir: dir} do
    bundle =
      quote do
        render = fn socket ->
          socket.assigns
          |> Map.put(:__changed__, nil)
          |> ThermostatLive.render()
          |> Phoenix.HTML.Safe.to_iodata()
          |> IO.iodata_to_binary()
        end

        {:ok, socket} = ThermostatLive.mount(%{}, %{}, %Phoenix.LiveView.Socket{})
        initial_html = render.(socket)

        {:noreply, hotter} = ThermostatLive.handle_event("inc_temperature", %{}, socket)
        hotter_html = render.(hotter)

        {:noreply, colder} = ThermostatLive.handle_event("dec_temperature", %{}, socket)
        colder_html = render.(colder)

        %{initial: initial_html, hotter: hotter_html, colder: colder_html}
      end
      |> AtomVM.compile_quoted()

    assert %{initial: initial_html, hotter: hotter_html, colder: colder_html} =
             AtomVM.run(bundle, dir)

    # Same behaviour the view exhibits in the browser, now produced by AtomVM:
    assert initial_html =~ "Current temperature: 25°C"
    assert initial_html =~ "Country: Poland"
    assert hotter_html =~ "Current temperature: 26°C"
    assert colder_html =~ "Current temperature: 24°C"
  end
end
