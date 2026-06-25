# Implementation Notes for LLV Navigation with Params

## Problem Statement

LocalLiveView needs to support URL-based navigation and parameter tracking for:
- Multi-step forms (current demo use case)
- Deep linking into app states
- Browser history (back/forward buttons)
- Bookmarkable views

## Current Architecture

```
URL: /?step=1
  ↓
CheckoutLive (Phoenix LiveView)
  ├─ handles mount/handle_params from Phoenix Router
  ├─ displays step indicator
  └─ renders <.local_live_view id={id} view="CheckoutLive" />
       ↓
       CheckoutLive (LocalLiveView in WASM)
         ├─ independent state (form_data, current_step)
         ├─ handles mount/handle_params (but currently only on initial mount)
         └─ renders step forms

```

## What Works Now

1. **LocalLiveView receives initial params on mount:**
   ```elixir
   def mount(_params, _session, socket) do
     # Gets called once when component is created
     # _params includes initial route params
   end
   ```

2. **LocalLiveView has handle_params callback:**
   ```elixir
   def handle_params(%{"step" => step}, _uri, socket) do
     # Already defined in local/lib/checkout_live.ex
     # But only called on initial mount, not on param changes
   end
   ```

3. **Client-side navigation works:**
   ```elixir
   defp navigate_to(_socket, step) do
     Popcorn.Wasm.run_js("""
     window.history.pushState({}, "", "/?step=" + args.step);
     """, %{step: step})
   end
   ```

## What Needs Implementation

### 1. Param Change Detection (Client-side)

**File:** `assets/local_live_view.js` (or similar)

```javascript
// Detect URL changes
window.addEventListener('popstate', (event) => {
  // Get new params from URL
  const params = new URLSearchParams(window.location.search);
  // Notify local live view
  sendToLLV({event: 'handle_params', params: Object.fromEntries(params)});
});

// Also detect when we use history.pushState
const originalPushState = window.history.pushState;
window.history.pushState = function(...args) {
  const result = originalPushState.apply(this, args);
  // Notify local live view of param changes
  return result;
};
```

### 2. LocalLiveView Dispatcher Update

**File:** `local-live-view/lib/local_live_view/dispatcher.ex`

Currently passes params only on initial view startup. Need to add:

```elixir
defp handle_wasm(
  {:wasm_call, %{"id" => id, "event" => "handle_params", "payload" => new_params}},
  state
) do
  pid = Map.get(state.views, id)
  send(pid, {:handle_params, new_params})
  {:resolve, :ok, state}
end
```

### 3. LocalLiveView.Server Update

**File:** `local-live-view/lib/local_live_view/server.ex`

Add handler for param updates:

```elixir
def handle_info({:handle_params, new_params}, %{socket: socket} = state) do
  lifecycle = Lifecycle.stage_info(socket, socket.view, :handle_params, 3)
  
  socket
  |> Utils.call_handle_params!(socket.view, lifecycle.exported?, new_params)
  |> handle_result({:handle_params, 3, nil}, state)
end
```

### 4. Update LocalLiveView Module

**File:** `local-live-view/lib/local_live_view/local_live_view.ex`

Ensure `handle_params/3` is listed in callbacks:

```elixir
@callback handle_params(
            params :: unsigned_params(),
            uri :: binary(),
            socket :: Socket.t()
          ) ::
            {:noreply, Socket.t()} |
            {:noreply, Socket.t(), keyword()}
```

## Testing the Implementation

Once implemented, the checkout demo should:

1. ✓ Load step 1: `/?step=1`
2. ✓ Fill shipping info
3. ✓ Click "Next" → URL changes to `/?step=2`
4. ✓ `handle_params(%{"step" => "2"}, ...)` is called
5. ✓ Step 2 payment form renders
6. ✓ **All data still preserved** from step 1 ✓
7. ✓ Browser back button → `/?step=1` → data still there
8. ✓ Parent step indicator updates in real-time

## Benefits of This Approach

1. **No server roundtrips for navigation** - everything client-side
2. **Bookmarkable states** - URL reflects current state
3. **Back/forward button support** - browser history works
4. **Offline capable** - only syncs on completion
5. **Familiar pattern** - mirrors Phoenix LiveView's handle_params

## Related Files

- Example: `/Users/marcel/Documents/swm/popcorn/examples/local-lv-checkout/`
- LocalLiveView lib: `/Users/marcel/Documents/swm/popcorn/local-live-view/lib/`
- Demo instructions: `examples/local-lv-checkout/DEMO.md`
