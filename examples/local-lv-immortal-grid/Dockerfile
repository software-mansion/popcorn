ARG ELIXIR_VERSION=1.17.3
ARG OTP_VERSION=27.2
ARG DEBIAN_VERSION=bookworm-20241202

ARG BUILDER_IMAGE="hexpm/elixir:${ELIXIR_VERSION}-erlang-${OTP_VERSION}-debian-${DEBIAN_VERSION}-slim"
ARG RUNNER_IMAGE="debian:bookworm-slim"

# ── Build stage ───────────────────────────────────────────────────────────────
FROM ${BUILDER_IMAGE} AS builder

RUN apt-get update -y && apt-get install -y --no-install-recommends build-essential git \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

RUN mix local.hex --force && mix local.rebar --force

ENV MIX_ENV="prod"

# Fetch deps first (cached layer)
COPY mix.exs mix.lock ./
RUN mix deps.get --only $MIX_ENV
RUN mkdir config

# Copy compile-time config before deps.compile so config changes
# invalidate the deps cache but not the source cache
COPY config/config.exs config/prod.exs config/
RUN mix deps.compile

RUN mix tailwind.install --if-missing

# Copy pre-built JS/WASM static assets (built locally via `mix setup`)
COPY priv priv/

# local/lib is referenced by assets/css/app.css via @source "../../local/lib"
COPY local/lib local/lib/

COPY lib lib/
COPY assets assets/

RUN mix compile

RUN mix tailwind immortal_grid --minify
RUN mix phx.digest

# Changes to runtime.exs don't require recompiling the code
COPY config/runtime.exs config/

RUN mix release

# ── Runtime stage ─────────────────────────────────────────────────────────────
FROM ${RUNNER_IMAGE} AS final

RUN apt-get update -y && apt-get install -y --no-install-recommends \
    libstdc++6 openssl libncurses6 locales ca-certificates \
    && rm -rf /var/lib/apt/lists/*

RUN sed -i '/en_US.UTF-8/s/^# //g' /etc/locale.gen && locale-gen
ENV LANG=en_US.UTF-8 LANGUAGE=en_US:en LC_ALL=en_US.UTF-8

WORKDIR /app
RUN chown nobody /app

ENV MIX_ENV="prod"
ENV PHX_SERVER="true"

COPY --from=builder --chown=nobody:root /app/_build/prod/rel/immortal_grid ./

USER nobody

CMD ["/app/bin/immortal_grid", "start"]
