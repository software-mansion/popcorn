# Example

Minimal Popdoc fixture for manual docs checks.

<!-- popcorn:eval -->

```elixir
Example.hello()
Example.add(20, 22)
```

Build the docs with:

```bash
mix deps.get
mix docs
mix popdoc.server
```

Then open `http://localhost:4000`.
