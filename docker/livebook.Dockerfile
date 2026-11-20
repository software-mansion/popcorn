ARG POPCORN_BASE_IMAGE=ghcr.io/software-mansion/popcorn-base
ARG POPCORN_BASE_TAG=latest
FROM ${POPCORN_BASE_IMAGE}:${POPCORN_BASE_TAG} AS build_livebook

# With the base's crypto runtime. `mix cook` builds public/ for a static host.
WORKDIR /build/popcorn/examples/livebook
RUN mise exec erlang@"${ERLANG_VERSION}" elixir@"${ELIXIR_VERSION}" -- \
    mix deps.get
RUN mise exec erlang@"${ERLANG_VERSION}" elixir@"${ELIXIR_VERSION}" -- \
    mix cook

FROM nginx:alpine AS runtime
COPY --from=build_livebook  /build/popcorn/docker/livebook.nginx.conf /etc/nginx/nginx.conf
COPY --from=build_livebook  /build/popcorn/examples/livebook/public /usr/share/nginx/html
EXPOSE 8080
