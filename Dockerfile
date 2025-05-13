FROM hexpm/elixir:1.17.3-erlang-26.0.2-debian-bookworm-20250428-slim

# Emscripten
RUN apt-get update && apt-get -y install git python3 xz-utils && \
    git clone https://github.com/emscripten-core/emsdk.git && cd emsdk && \
    ./emsdk install latest && ./emsdk activate latest && . ./emsdk_env.sh

ENV PATH=$PATH:/emsdk/upstream/emscripten/

# FissionVM deps
RUN apt-get update && apt-get -y install cmake gperf libmbedtls-dev zlib1g-dev

# IEx WASM

COPY iex_wasm /iex_wasm
COPY popcorn /popcorn
COPY FissionVM /FissionVM

RUN cd /iex_wasm && \
    export MIX_ENV=prod && \
    rm -rf _build deps static/wasm && mix deps.get && mix deps.compile && \
    ATOMVM_SOURCE_PATH=/FissionVM mix popcorn.build_runtime --out-dir static/wasm --target wasm && \
    mix compile && \
    # Run server script to download and cache its deps
    echo "\n" | elixir server.exs

# Run tests
RUN apt-get update && apt-get -y install nodejs && \
    cd /iex_wasm && export MIX_ENV=test && mix playwright.install && mix test

CMD elixir iex_wasm/server.exs
