# CompareLiveViews

This is a demo project that illustrates the differences between Popcorn's Local Live View and Phoenix Live View behaviors.

The page is split into two halves, each covered by a modal. One modal is implemented using Local LiveView and the other with Phoenix LiveView.

The local part of the project is located in the `local` directory.

The most important files to look at are:

`compare_live_views/lib/compare_live_views_web/live/demo_modal_online.ex` - Phoenix LiveView that represents online part of the demo and is rendered on the left side of the page.

`compare_live_views/local/lib/demo_modal_offline.ex` - LocalLiveView that represents offline part of the demo and is rendered on the right side of the page.

`compare_live_views/lib/compare_live_views_web/controllers/page_html/home.html.heex` - home page that renders both views.

To start your server:

* To install and setup dependencies (both online and offline part), run:
```
mix setup
``` 
_The popcorn output files will be generated into_ `priv/static/local_live_view` _also the local_live_view.js script is included in the head section of_ `lib/compare_live_views_web/components/layouts/root.html.heex`

* Start Phoenix endpoint with `mix phx.server` or inside IEx with `iex -S mix phx.server`

Now you can visit [`localhost:4000`](http://localhost:4000) from your browser.

_The project enables Phoenix LiveView latency simulation by using `liveSocket.enableLatencySim(1000)` to show response time difference between the LiveViews. It can be later changed by calling `liveSocket.enableLatencySim` in the console._
