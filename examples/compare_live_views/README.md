# CompareLiveViews

This is a demo project that illustrates the differences between Popcorn's Local Live View and Phoenix Live View behaviors.

The page is split to two halves each covered by a modal. One of the modal is implemented using Local Live View and another by Phoenix Live View.

To start your server:

* Generate the offline part of the project by running:
  ```
  cd priv/demo_modal_offline
  mix popcorn.cook
  cd ../..
  ```
* Run `mix setup` to install and setup dependencies
* Start Phoenix endpoint with `mix phx.server` or inside IEx with `iex -S mix phx.server`

Now you can visit [`localhost:4000`](http://localhost:4000) from your browser.
