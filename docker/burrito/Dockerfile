ARG POPCORN_BASE_IMAGE=ghcr.io/software-mansion/popcorn-base
ARG POPCORN_BASE_TAG=latest

FROM ${POPCORN_BASE_IMAGE}:${POPCORN_BASE_TAG} AS builder

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

COPY examples/local-lv-burrito/local/ local/
RUN cd local && mix deps.get && MIX_TARGET=wasm mix popcorn.cook

RUN LLV_PROJECT_DIR=$(pwd) node /build/popcorn/local-live-view/assets/bundle.mjs

COPY examples/local-lv-burrito/lib/ lib/
COPY examples/local-lv-burrito/assets/ assets/
RUN mix compile
RUN cd assets && CI=true pnpm run build
RUN mix tailwind burrito --minify
RUN mix phx.digest

COPY examples/local-lv-burrito/config/runtime.exs config/
RUN mix release

# ── Runtime stage ─────────────────────────────────────────────────────────────
FROM ubuntu:noble AS final

RUN apt-get update -y && apt-get install -y --no-install-recommends \
    libstdc++6 openssl libncurses6 locales ca-certificates \
    && rm -rf /var/lib/apt/lists/*

RUN locale-gen en_US.UTF-8
ENV LANG=en_US.UTF-8 LANGUAGE=en_US:en LC_ALL=en_US.UTF-8

WORKDIR /app
RUN chown nobody /app

ENV MIX_ENV="prod"
ENV PHX_SERVER="true"

COPY --from=builder --chown=nobody:root \
    /build/popcorn/examples/local-lv-burrito/_build/prod/rel/burrito ./

USER nobody

CMD ["/app/bin/burrito", "start"]
