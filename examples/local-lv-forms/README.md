# Local Live View Form Demo

## Description

A demo project implementing Phoenix forms using LocalLiveView.

The demo form is built as an HTML page with a single form and two rendered lists of users (usernames and emails).

One list is stored in the local LiveView assigns, while the other is stored in the Phoenix LiveView assigns. These will be referred to as the local list and the server list, respectively.

The form handles `phx-change` and `phx-submit` LocalLiveView events.
Validation is driven by an Ecto `embedded_schema` (`local/lib/user.ex`), exactly as you would
in a regular Phoenix LiveView: the `validate` event builds an `Ecto.Changeset` and the `save`
event commits it with `Ecto.Changeset.apply_action/2`. The changeset enforces username length,
email format, and rejects usernames/emails already present in the list.

On mount, both the local and Phoenix LiveViews synchronize their state (list of users).

On submit, a new user is added to the local list and also sent to the server list.

**Since both lists are stored in the runtime, the demo illustrates the persistent nature of a local-first approach. You can shut down the server to see the server list synchronize from its local counterpart upon restart. Conversely, refreshing the page will empty the local list, which then synchronizes from the server.**

## Technical Overview

The local part of the project is located in the `local` directory.

The most important files to look at are:

[`local/lib/form_demo_local.ex`](local/lib/form_demo_local.ex) -
LocalLiveView that implements the form demo.

[`lib/form_demo_web/controllers/page_html/home.html.heex`](lib/form_demo_web/controllers/page_html/home.html.heex) - home page that renders both views.

_The LocalLiveView project needs a few things to run properly:_

* `LocalLiveView.Component` imported in `lib/form_demo_web.ex`
* `llv.build` in the setup alias in `mix.exs`
* Generated assets served from `priv/static/assets/js/popcorn/`

## Usage

From the repository root:

```bash
pnpm install
mise run dev --example local-lv-forms
```

or directly from the example directory:

```bash
mix dev
```

and visit [localhost:4000](http://localhost:4000)
