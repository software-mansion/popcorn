ARG POPCORN_BASE_IMAGE=ghcr.io/software-mansion/popcorn-base
ARG POPCORN_BASE_TAG=latest

FROM ${POPCORN_BASE_IMAGE}:${POPCORN_BASE_TAG} AS builder

WORKDIR /build/popcorn
COPY . .

RUN --mount=type=cache,id=pnpm,target=/pnpm/store \
    mise trust /build/popcorn && \
    pnpm install --store-dir /pnpm/store --frozen-lockfile \
      --filter ./popcorn/js... --filter ./local-live-view... && \
    pnpm -F ./popcorn/js build

WORKDIR /build/popcorn/examples/local-lv-burrito

RUN mix deps.get --only prod && \
    mix deps.compile && \
    mix tailwind.install --if-missing && \
    mix esbuild.install --if-missing && \
    mix llv.build && \
    mix compile && \
    mix esbuild burrito --minify && \
    mix tailwind burrito --minify && \
    mix phx.digest && \
    mix release

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
