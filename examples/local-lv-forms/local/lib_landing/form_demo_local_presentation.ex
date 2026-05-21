defmodule FormDemoLocalPresentation do
  use LocalLiveView

  defdelegate render(assigns), to: FormDemoLocal

  def mount(params, session, socket) do
    result = FormDemoLocal.mount(params, session, socket)
    {:ok, new_socket} = result

    Popcorn.Wasm.send_event("llv_presentation", %{
      block: nil,
      event: "mount",
      assigns: presentation_assigns(new_socket)
    })

    result
  end

  def handle_event(event, params, socket)
      when event in ["validate", "save", "generate_random"] do
    effective_socket =
      case socket.assigns[:_pending_cur] do
        nil -> socket
        pending -> assign(socket, Map.to_list(pending))
      end

    {:noreply, updated} = FormDemoLocal.handle_event(event, params, effective_socket)

    Popcorn.Wasm.send_event("llv_presentation", %{
      block: event,
      event: event,
      assigns: presentation_assigns(updated)
    })

    new_pending = %{
      form: updated.assigns.form,
      errors: updated.assigns.errors,
      disabled: updated.assigns.disabled,
      users: updated.assigns.users
    }

    {:noreply,
     socket
     |> assign(:_pending_prev, socket.assigns[:_pending_cur])
     |> assign(:_pending_cur, new_pending)}
  end

  def handle_info({:js_push, "llv_ack", _}, socket) do
    case {socket.assigns[:_pending_prev], socket.assigns[:_pending_cur]} do
      {%{form: form, errors: errors, disabled: disabled, users: users}, _} ->
        {:noreply,
         socket
         |> assign(:form, form)
         |> assign(:errors, errors)
         |> assign(:disabled, disabled)
         |> assign(:users, users)
         |> assign(:_pending_prev, nil)}

      {nil, %{form: form, errors: errors, disabled: disabled, users: users}} ->
        {:noreply,
         socket
         |> assign(:form, form)
         |> assign(:errors, errors)
         |> assign(:disabled, disabled)
         |> assign(:users, users)
         |> assign(:_pending_cur, nil)}

      _ ->
        {:noreply, socket}
    end
  end

  def handle_event(event, params, socket) do
    FormDemoLocal.handle_event(event, params, socket)
  end

  defp presentation_assigns(socket) do
    assigns = socket.assigns
    form_params = assigns.form.params

    %{
      username: Map.get(form_params, "username", ""),
      email: Map.get(form_params, "email", ""),
      errors: length(assigns.errors),
      disabled: assigns.disabled,
      users: length(assigns.users)
    }
  end
end
