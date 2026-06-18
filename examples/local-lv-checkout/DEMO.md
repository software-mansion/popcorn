# Multi-Step Checkout Demo

This demo shows how to implement **multi-step forms in LocalLiveView** with proper navigation and state persistence.

## Key Features Demonstrated

### 1. **Parameter-based Navigation**
- URL tracks current step: `/?step=1`, `/?step=2`, `/?step=3`
- No server roundtrips for step navigation
- Browser back/forward buttons work
- State is preserved across steps

### 2. **State Persistence**
- All form data persists in `form_data` assigns
- Switching between steps doesn't lose data
- Form data available for preview on final step

### 3. **Step Validation**
- Each step validates before allowing progression
- Errors displayed inline
- Next button disabled if step has errors

### 4. **Proper handle_params Usage**
```elixir
def handle_params(%{"step" => step_str}, _uri, socket) do
  step = String.to_integer(step_str)
  {:noreply, assign(socket, current_step: step)}
end
```

## Architecture

```
CheckoutLive (Parent - Phoenix LiveView)
  └─ handles query param changes
  └─ displays step indicator
  └─ renders CheckoutFormComponent

CheckoutFormComponent (LiveComponent)
  └─ renders CheckoutLive local live view

CheckoutLive (Local - LocalLiveView in WASM)
  └─ renders step form
  └─ handles field updates
  └─ handles navigation
  └─ persists state across steps
```

## Running the Demo

```bash
cd examples/local-lv-checkout
mix deps.get
mix build
mix dev
```

Visit http://localhost:4000

## Testing Flow

1. **Step 1 (Shipping)**: Fill in email, name, address, city
2. **Step 2 (Payment)**: Enter card details
3. **Step 3 (Confirmation)**: Review order
4. **Complete**: Submit order

**Try these scenarios:**
- Navigate back/forward - data persists ✓
- Try to proceed without filling fields - validation blocks ✓
- Use browser back button - step updates correctly ✓
- Refresh page - redirects to step 1 (initial state) ✓

## Key Implementation Details

### In Local Live View (CheckoutLive):

1. **Mount initializes empty form data:**
   ```elixir
   def mount(_params, _session, socket) do
     {:ok, assign(socket,
       current_step: 1,
       form_data: %{"email" => "", "name" => "", ...},
       errors: []
     )}
   end
   ```

2. **handle_params receives step from URL:**
   ```elixir
   def handle_params(%{"step" => step_str}, _uri, socket) do
     step = String.to_integer(step_str)
     {:noreply, assign(socket, current_step: step)}
   end
   ```

3. **Navigation uses push_navigate:**
   ```elixir
   def handle_event("next_step", _params, socket) do
     if validate_step(socket.assigns.current_step, socket.assigns.form_data) == [] do
       {:noreply, socket
         |> assign(current_step: step + 1)
         |> push_navigate(to: "/?step=#{step + 1}")}
     else
       {:noreply, assign(socket, errors: errors)}
     end
   end
   ```

## Current Implementation Status

### What Works ✓
- Local live view receives initial step from mount
- Form data persists in assigns across all steps
- Validation works per step
- Navigation updates URL via `window.history.pushState`
- Can navigate back/forward in browser history

### Current Limitation ⚠️
- **Parent step indicator doesn't update in real-time** after navigation
  - Reason: Parent is normal LiveView (server-side WebSocket), Local is WASM (client-side)
  - URL changes but parent doesn't get notified
  - This is the key problem that `handle_params` implementation would solve

### To Make This Fully Work

Implement `handle_params` support in LocalLiveView so:
1. When URL changes (via `history.pushState`), local live view is notified
2. Parent also receives the change via its own `handle_params`
3. Both can update simultaneously without server roundtrips

This requires:
- [ ] Add `handle_params/3` callback to LocalLiveView module
- [ ] Modify dispatcher to pass param updates to local live view
- [ ] Wire up URL change detection in client JS
- [ ] Test parent + local coordination

## What This Shows About LLV Navigation

This demo demonstrates:
- ✓ Local live views can implement `handle_params/3`
- ✓ State can be maintained across param changes
- ✓ URL can be updated without full page reload (via `history.pushState`)
- ✓ Complex multi-step workflows are possible
- ✓ Form data persists across steps
- ✗ But: Parent and local live views don't auto-sync on URL changes

**This validates the approach** - we just need to implement the param notification mechanism in the dispatcher.
