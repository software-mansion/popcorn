---
title: Elixir API
description: Reference of APIs available in Elixir
---

`Popcorn.Wasm` module handles communication with JS and provides APIs for running JavaScript code from Elixir.

## Types

### `TrackedObject.t()`

Represents opaque reference to JS value. Automatically cleaned up when garbage collected on Elixir side.

## Core Functions

### `register(main_process_name)`

Notifies JS that Elixir side finished initializing. Can be called only once.

**Parameters:**

- `main_process_name` (atom or string) - Name of the main process that will receive JS messages by default.

**Returns:** `:ok`.

**Example:**

```elixir
Popcorn.Wasm.register(:main)
```

### `handle_message!(raw_message, handler)`

Deserializes raw message from JS and calls handler. For `:wasm_call` messages, settles the promise with a value. Uses `is_wasm_message/1` guard to validate input.

**Parameters:**

- `raw_message` (raw_message()) - Raw message from JS.
- `handler` (function) - Function that processes the parsed message.

**Returns:** Handler result

**Handler arguments**

Popcorn passes one of the following to the `handler`:

- `{:wasm_call, message}` – when JS used `popcorn.call()`.
- `{:wasm_cast, cast_message}` – when JS used `popcorn.cast()`.
- `{:wasm_event, event_name, event_data, custom_data}` – when event registered via `register_event_listener/2` was fired.

**Handler return values**

Popcorn expects handler to return one of the following:

- For calls: `{:resolve | :reject, promise_reply, result}` tuple.
- For casts: `result` term.
- For events: `result` term.

**Example**

```elixir
Popcorn.Wasm.handle_message!(raw_msg, fn
  {:wasm_call, data} -> {:resolve, "success", data}
  {:wasm_cast, data} -> :ok
  {:wasm_event, :click, event_data, custom} -> handle_click(event_data)
end)
```

**Notes:**

- Use `is_wasm_message/1` guard to check if message should be handled. Useful in genservers (`handle_info`).

### `is_wasm_message(msg)` (guard)

Guard that returns true if argument is a message from JS side.

## Running JS

### `run_js(function, args, opts)`

Runs JS code in the Wasm iframe context and returns tracked objects. Also comes as raising variation that returns results directly.

**Parameters:**

- `function` (string) - JS function as string that returns array of values to track.
- `args` (map, optional) - Arguments passed to JS function. Default: `%{}`.
- `opts` (keyword, optional) - Options keyword list. Default: `[]`.

**Options:**

- `:return` (atom) - Return type: `:ref`, `:refs`, `:value`, `:values`. Default: `:ref`.

**Returns:** `{:ok, result}` or `{:error, reason}`.

**JS function**

Passed JS function receives object with properties that can be used:

- `args` – deserialized args passed from Elixir.
- `wasm` – Emscripten's Wasm module. Can be used to send messages to Elixir. See notes.
- `iframeWindow` – original iframe window. See notes.

JS function should return either single value, array of values or nothing. See notes.

**Example**

```elixir
Popcorn.Wasm.run_js("""
({ args }) => {
  const element = document.getElementById(args.id);
  return [element];
}
""", %{id: "button1"})
# => {:ok, %TrackedObject{}}
```

**Notes:**

- `window` and `document` are shadowed to reference parent window for DOM access. If you need to use window from iframe, use `iframeWindow` property.
- If you plan to return multiple objects, you should use `return: :refs` or `return: :values` instead of calling `run_js` in a loop. This makes performance penalty of invoking JS and serialization/deserialization smaller.
- `wasm` argument passed to JS function is a emscripten's instantiated wasm module.
  It does not have same crash and timeout resiliency as `Popcorn` object. This matters when calling `wasm.call()` which may hang the runtime if used improperly.

### `get_tracked_values(refs)`

Takes tracked objects and deserializes JS values to Elixir terms. Also comes as raising variation that returns results directly.

**Parameters:**

- `refs` (list) - List of `TrackedObject.t()`.

**Returns:** List of `{:ok, value}` or `{:error, reason}` tuples.

## Event Handling

### `register_event_listener(event_name, opts)`

Registers event listener for DOM element. Events are sent as `:wasm_event` messages.

**Parameters:**

- `event_name` (atom) - Event name (e.g., `:click`, `:keydown`).
- `opts` (keyword) - Options keyword list.

**Options:**

- `:event_keys` (list) - Event property atoms to capture. Default: `[]`.
- `:target_node` (TrackedObject.t()) - DOM element to attach listener to.
- `:event_receiver` (string) - Process name to receive events.
- `:custom_data` (any) - Custom data sent with events. Default: `nil`.

**Returns:** `{:ok, TrackedObject.t()}` - Cleanup reference. See `unregister_event_listener/1`.

**Example**

```elixir
{:ok, cleanup_ref} = Popcorn.Wasm.register_event_listener(:click, [
  target_node: button_ref,
  event_receiver: "click_handler",
  event_keys: [:clientX, :clientY],
  custom_data: %{id: "button1"}
])
```

### `unregister_event_listener(ref)`

Unregisters event listener using cleanup reference.

**Parameters:**

- `ref` (TrackedObject.t()) - Cleanup reference from `register_event_listener/2`.

**Returns:** `{:ok, result}` or `{:error, reason}`.

## Low Level Functions

### `parse_message!(raw_message)`

Deserializes message received from JS side into structured format.

**Parameters:**

- `raw_message` (raw_message()) - Raw message tuple from JS.

**Returns:**

- `{:wasm_call, data, promise}` - For call messages.
- `{:wasm_cast, data}` - For cast messages.
- `{:wasm_event, event_name, data, custom}` - For DOM events.

### `resolve(term, promise)`

Resolves JS promise with serializable value.

**Parameters:**

- `term` (any) - Serializable term to send.
- `promise` (promise()) - Promise reference.

**Returns:** `:ok`.

### `reject(term, promise)`

Rejects JS promise with serializable value.

**Parameters:**

- `term` (any) - Serializable term to send.
- `promise` (promise()) - Promise reference.

**Returns:** `:ok`.
