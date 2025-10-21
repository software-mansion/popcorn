# CompareLiveViews

This is a demo project that illustrates the differences between Popcorn's Local Live View and Phoenix Live View behaviors.

The page is split into two halves, each covered by a modal. One modal is implemented using Local LiveView and the other with Phoenix LiveView.

The local part of the project is located in `local` directory. It can be rebuilt and generate avm files by running:
```
cd local/compare_live_views_local
mix deps.get
mix popcorn.cook
cd ../..
```

_The popcorn output files will be generated into_ `priv/static/local_live_view` _also the local_live_view.js script is included in the head section of_ `lib/compare_live_views_web/components/layouts/root.html.heex`

To start your server:

* To install and setup dependencies (both online and offline part), run:
```
mix setup
``` 
* Start Phoenix endpoint with `mix phx.server` or inside IEx with `iex -S mix phx.server`

Now you can visit [`localhost:4000`](http://localhost:4000) from your browser.
