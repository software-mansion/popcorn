ARG POPCORN_BASE_IMAGE=ghcr.io/software-mansion/popcorn-base
ARG POPCORN_BASE_TAG=latest

# ── Kanban builder ────────────────────────────────────────────────────────────
FROM ${POPCORN_BASE_IMAGE}:${POPCORN_BASE_TAG} AS builder-kanban

COPY popcorn/elixir/ /build/popcorn/popcorn/elixir/
COPY local-live-view/ /build/popcorn/local-live-view/

WORKDIR /build/popcorn
RUN CI=true pnpm install --no-frozen-lockfile --shamefully-hoist

WORKDIR /build/popcorn/examples/local-lv-kanban

COPY examples/local-lv-kanban/mix.exs examples/local-lv-kanban/mix.lock ./
RUN mix deps.get --only prod
RUN mkdir -p config
COPY examples/local-lv-kanban/config/config.exs examples/local-lv-kanban/config/prod.exs config/
RUN mix deps.compile

RUN mix tailwind.install --if-missing
RUN mix esbuild.install --if-missing

COPY examples/local-lv-kanban/local/ local/
RUN cd local && mix deps.get && MIX_TARGET=wasm mix popcorn.cook

RUN mix llv.build

COPY examples/local-lv-kanban/lib/ lib/
COPY examples/local-lv-kanban/assets/ assets/
COPY examples/local-lv-kanban/priv/ priv/
RUN mix compile
RUN mix esbuild local_lv_kanban --minify
RUN mix tailwind local_lv_kanban --minify
RUN mix phx.digest

COPY examples/local-lv-kanban/config/runtime.exs config/
COPY examples/local-lv-kanban/rel/ rel/
RUN mix release

# ── Burrito builder ───────────────────────────────────────────────────────────
FROM ${POPCORN_BASE_IMAGE}:${POPCORN_BASE_TAG} AS builder-burrito

COPY popcorn/elixir/ /build/popcorn/popcorn/elixir/
COPY local-live-view/ /build/popcorn/local-live-view/

WORKDIR /build/popcorn
RUN CI=true pnpm install --no-frozen-lockfile --shamefully-hoist

WORKDIR /build/popcorn/examples/local-lv-burrito

COPY examples/local-lv-burrito/mix.exs examples/local-lv-burrito/mix.lock ./
RUN mix deps.get --only prod
RUN mkdir -p config
COPY examples/local-lv-burrito/config/config.exs examples/local-lv-burrito/config/prod.exs config/
RUN mix deps.compile

RUN mix tailwind.install --if-missing
RUN mix esbuild.install --if-missing

COPY examples/local-lv-burrito/local/ local/
RUN cd local && mix deps.get && MIX_TARGET=wasm mix popcorn.cook

RUN mix llv.build

COPY examples/local-lv-burrito/lib/ lib/
COPY examples/local-lv-burrito/assets/ assets/
RUN mix compile
RUN mix esbuild burrito --minify
RUN mix tailwind burrito --minify
RUN mix phx.digest

COPY examples/local-lv-burrito/config/runtime.exs config/
RUN mix release

# ── Pong builder ──────────────────────────────────────────────────────────────
FROM ${POPCORN_BASE_IMAGE}:${POPCORN_BASE_TAG} AS builder-pong

COPY popcorn/elixir/ /build/popcorn/popcorn/elixir/
COPY local-live-view/ /build/popcorn/local-live-view/

WORKDIR /build/popcorn
RUN CI=true pnpm install --no-frozen-lockfile --shamefully-hoist

WORKDIR /build/popcorn/examples/local-lv-pong

COPY examples/local-lv-pong/mix.exs examples/local-lv-pong/mix.lock ./
RUN mix deps.get --only prod
RUN mkdir -p config
COPY examples/local-lv-pong/config/config.exs examples/local-lv-pong/config/prod.exs config/
RUN mix deps.compile

RUN mix tailwind.install --if-missing
RUN mix esbuild.install --if-missing

COPY examples/local-lv-pong/local/ local/
RUN cd local && mix deps.get && MIX_TARGET=wasm mix popcorn.cook

RUN mix llv.build

COPY examples/local-lv-pong/lib/ lib/
COPY examples/local-lv-pong/assets/ assets/
COPY examples/local-lv-pong/priv/ priv/
RUN mix compile
RUN mix esbuild local_lv_pong --minify
RUN mix tailwind local_lv_pong --minify
RUN mix phx.digest

COPY examples/local-lv-pong/config/runtime.exs config/
RUN mix release

# ── Runtime stage ─────────────────────────────────────────────────────────────
# Single self-contained container: the demo apps, an embedded Postgres
# (shared; only kanban uses it) and nginx routing /kanban, /burrito and /pong.
# No volume: demo data is disposable and seeds repopulate it on redeploy.
FROM ubuntu:noble AS final

RUN apt-get update -y && apt-get install -y --no-install-recommends \
    libstdc++6 openssl libncurses6 locales ca-certificates \
    postgresql-16 nginx \
    && rm -rf /var/lib/apt/lists/*

RUN locale-gen en_US.UTF-8
ENV LANG=en_US.UTF-8 LANGUAGE=en_US:en LC_ALL=en_US.UTF-8

WORKDIR /app
RUN chown nobody /app

ENV MIX_ENV="prod"
ENV PHX_SERVER="true"

COPY --from=builder-kanban --chown=nobody:root \
    /build/popcorn/examples/local-lv-kanban/_build/prod/rel/local_lv_kanban ./kanban/
COPY --from=builder-burrito --chown=nobody:root \
    /build/popcorn/examples/local-lv-burrito/_build/prod/rel/burrito ./burrito/
COPY --from=builder-pong --chown=nobody:root \
    /build/popcorn/examples/local-lv-pong/_build/prod/rel/local_lv_pong ./pong/

COPY docker/demos/nginx.conf /etc/nginx/nginx.conf.template
COPY docker/demos/index.html /var/www/demos/index.html
COPY --chmod=755 docker/demos/entrypoint.sh /entrypoint.sh

# Root: the entrypoint starts Postgres and nginx, then drops to nobody for
# the apps.
CMD ["/entrypoint.sh"]
