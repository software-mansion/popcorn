# Local Live View Form Demo

The demo project implementing phoenix forms using LocalLiveView.

The demo form is built as an html page with a single form and a rendered list of users (usernames and emails). 
List of users is stored in local live view process memory.
The form handles onchange and onsubmit LocalLiveView events.
The change event triggers validate function that looks through already saved users and makes sure that
new username or email isn't already present in the users list. The submit event adds a new user to the user list.

The local part of the project is located in the `local` directory.

The most important files to look at are:

`compare_live_views/local/lib/form_demo_local.ex` - 
LocalLiveView that implements the form demo.

`compare_live_views/lib/compare_live_views_web/controllers/page_html/home.html.heex` - home page that renders both views.

_The local_live_view project needs few things to run properly:_
* `local_live_view` directory added to `static_paths` function in `form_demo/lib/form_demo_web.ex`
* proper `out_dir` property in `form_demo/local/config/config.exs`
* `/priv/static/local_live_view/` line added to `form_demo/.gitignore` file

To start your server:

* To install and setup dependencies (both online and offline part), run:
```
mix setup
``` 
_The popcorn output files will be generated into_ `priv/static/local_live_view` _also the local_live_view.js script is included in the head section of_ `lib/compare_live_views_web/components/layouts/root.html.heex`

* Start Phoenix endpoint with `mix phx.server` or inside IEx with `iex -S mix phx.server`

Now you can visit [`localhost:4000`](http://localhost:4000) from your browser.
