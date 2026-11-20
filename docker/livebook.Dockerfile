ARG POPCORN_BASE_IMAGE=ghcr.io/software-mansion/popcorn-base
ARG POPCORN_BASE_TAG=latest
FROM ${POPCORN_BASE_IMAGE}:${POPCORN_BASE_TAG} AS build_livebook

WORKDIR /build/popcorn
COPY . .

RUN --mount=type=cache,id=pnpm,target=/pnpm/store \
    mise trust /build/popcorn && \
    pnpm install --store-dir /pnpm/store --frozen-lockfile \
      --filter ./popcorn/js... && \
    pnpm -F ./popcorn/js build

# With the crypto runtime, which the build context has in popcorn/out.
# `mix cook` builds public/ for a static host.
WORKDIR /build/popcorn/examples/livebook
RUN mix deps.get && \
    mix cook

FROM nginx:alpine AS runtime
COPY --from=build_livebook /build/popcorn/docker/livebook.nginx.conf /etc/nginx/nginx.conf
COPY --from=build_livebook /build/popcorn/examples/livebook/public /usr/share/nginx/html
EXPOSE 8080
