FROM ubuntu:noble

ENV MIX_ENV=prod
ENV LC_ALL=C.UTF-8
ENV CI=true
ENV DEBIAN_FRONTEND='noninteractive'
ENV PATH="/root/.local/share/mise/shims:$PATH"

WORKDIR /build/
RUN mkdir -p popcorn out
RUN apt-get -y update && \
    apt-get -y install --no-install-recommends \
    ca-certificates git curl zlib1g-dev \
    make gcc g++ libssl-dev libncurses-dev \
    python3 xz-utils gpg wget && \
    rm -rf /var/lib/apt/lists/*

# install mise
RUN install -dm 755 /etc/apt/keyrings && \
    wget -qO - https://mise.jdx.dev/gpg-key.pub | gpg --dearmor > /etc/apt/keyrings/mise-archive-keyring.gpg && \
    echo "deb [signed-by=/etc/apt/keyrings/mise-archive-keyring.gpg arch=amd64,arm64] https://mise.jdx.dev/deb stable main" > /etc/apt/sources.list.d/mise.list && \
    apt-get update && \
    apt-get install -y --no-install-recommends mise && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /build/popcorn
COPY mise.toml ./
RUN mise trust /build/popcorn && \
    mise install node pnpm erlang elixir && \
    mise exec -- mix local.rebar --force && \
    mise exec -- mix local.hex --if-missing --force
