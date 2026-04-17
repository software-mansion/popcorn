# CompareLiveViews

This is a demo project that illustrates the differences between Popcorn's Local Live View and Phoenix Live View behaviors.

The page is split into two halves, each covered by a modal. One modal is implemented using Local LiveView and the other with Phoenix LiveView.

The local part of the project is located in the `local` directory.

The most important files to look at are:

`compare_live_views/lib/compare_live_views_web/live/demo_modal_online.ex` - Phoenix LiveView that represents online part of the demo and is rendered on the left side of the page.

`compare_live_views/local/lib/demo_modal_offline.ex` - LocalLiveView that represents offline part of the demo and is rendered on the right side of the page.

`compare_live_views/lib/compare_live_views_web/controllers/page_html/home.html.heex` - home page that renders both views.

## Usage

From the repository root:

```bash
pnpm install
mise run dev --example local-lv-compare
```

or directly from the example directory:

```bash
mix dev
```

and visit [localhost:4000](http://localhost:4000)

_The project enables Phoenix LiveView latency simulation by using `liveSocket.enableLatencySim(1000)` to show response time difference between the LiveViews. It can be later changed by calling `liveSocket.enableLatencySim` in the console._
