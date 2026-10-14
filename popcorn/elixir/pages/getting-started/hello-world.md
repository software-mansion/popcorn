# Building a simple Popcorn app

> This guide assumes you have Popcorn installed in "Foo" application, with supervision tree. Change names to apropriate ones. See ["Installation"](installation.md) for installation instructions.

Let's create a small "Hello world!" example with Popcorn.

## Overview

To create a usable application that will load in the browser, you need:

- you app's code in Elixir.
- HTML that will be served to the browser.
- VM and standard library (Popcorn provides these).

Let's start with HTML. Our app will replace the page content from Elixir.

## Add a HTML template

Create a `public` directory. Our static assets will live there. We also need a directory where we will put the built app files. We will use `public/out` for that.

```bash
mkdir -p public public/out
# if you use Git for this example
echo 'public/out' >> .gitignore
```

Now, HTML template in `public/index.html`:

```html
<!-- exdoc:fold `<html> ...` | HTML structure -->
<!doctype html>
<html>
  <head>
    <meta charset="utf-8" />
    <meta name="viewport" content="width=device-width, initial-scale=1" />
    <meta name="color-scheme" content="light dark" />
  </head>
  <!-- exdoc:endfold -->
  <body>
    <p id="status">Starting Popcorn...</p>

    <script type="module">
      import { Popcorn } from "./out/index.mjs";

      const { ok, data, error } = await Popcorn.init();
      if (!ok) throw error;
    </script>
  </body>
</html>
```

It's a minimal HTML file. It contains a paragraph (`<p id="status">...</p>`) we will change in our application. It also has a Popcorn instance initialization (`await Popcorn.init()`).

Your project should look like this:

```plaintext
foo/
├── lib/
├── public/
│   ├── index.html
│   └── out/
├── test/
├── mix.exs
└── mix.lock
```

## Add a function to change the HTML

Edit the `lib/foo.ex` and add a function to run JavaScript from Elixir side:

```elixir
alias Popcorn.Wasm

def set_html do
  with :ok <- Wasm.await_ready() do
    Wasm.run_js!("""
      () => {
        const status = document.querySelector("#status");

        status.textContent = "Hello world!";
      }
    """)

    :ok
  end
end
```

It waits until JavasScript finishes initialization [^init] and runs JavaScript code to replace the paragraph's content.

Now, we need to call it from somewhere. A `Task` in `application.ex`'s supervision tree is a perfect place:

```diff
   @impl true
   def start(_type, _args) do
     children = [
-      # Starts a worker by calling: Foo.Worker.start_link(arg)
-      # {Foo.Worker, arg}
+      {Task, &Foo.set_html/0}
     ]

     # See https://hexdocs.pm/elixir/Supervisor.html
```

## Start the runtime

Start the local server:

```
mix popcorn.dev
```

and access [localhost:4000](localhost:4000). You should see "Hello world!" written on the page.

## What happened?

1. We initialized Popcorn in the JavaScript `<script>` tag, which booted the Erlang/OTP VM.
2. The VM started its supervision tree and `Task` with our code to replace HTML.
3. JavaScript side finished Popcorn initialization and `set_html()` ran our JavaScript snippet.

The `mix popcorn.dev` command also packaged our application to a tarball and included all needed files. Run `tree public` to see directory tree.

## Next

Now, you can play around with [Popcorn APIs](guides/js-apis.md) in that example app or check out a bit more complex project in ["Hello counters"](hello-counters.md).

[^init]: In Popcorn, JS waits until supervision tree is ready and then finishes initialization, while Elixir runs asynchrounously. Read more in ["Architecture: initialization"](architecture.md#initialization).
