ARG POPCORN_BASE_IMAGE=ghcr.io/software-mansion/popcorn-base
ARG POPCORN_BASE_TAG=latest

FROM ${POPCORN_BASE_IMAGE}:${POPCORN_BASE_TAG} AS builder

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
RUN mix release

# ── Runtime stage ─────────────────────────────────────────────────────────────
# Single self-contained container: the app plus an embedded Postgres, so the
# image deploys exactly like the burrito one (no extra services, no volume,
# no DATABASE_URL to provide). Demo data is disposable; seeds repopulate it.
FROM ubuntu:noble AS final

RUN apt-get update -y && apt-get install -y --no-install-recommends \
    libstdc++6 openssl libncurses6 locales ca-certificates postgresql-16 \
    && rm -rf /var/lib/apt/lists/*

RUN locale-gen en_US.UTF-8
ENV LANG=en_US.UTF-8 LANGUAGE=en_US:en LC_ALL=en_US.UTF-8

WORKDIR /app
RUN chown nobody /app

ENV MIX_ENV="prod"
ENV PHX_SERVER="true"

COPY --from=builder --chown=nobody:root \
    /build/popcorn/examples/local-lv-kanban/_build/prod/rel/local_lv_kanban ./

COPY --chmod=755 docker/kanban/entrypoint.sh /entrypoint.sh

# Root: the entrypoint starts Postgres, then drops to nobody for the app.
CMD ["/entrypoint.sh"]
