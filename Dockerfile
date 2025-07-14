FROM ubuntu:noble AS build

ENV ERLANG_VERSION="26.0.2"
ENV ELIXIR_VERSION="1.17.3-otp-26"
ENV NODE_VERSION="22"
ENV EMSDK_VERSION="4.0.8"
ENV MIX_ENV=prod
ENV LC_ALL=C.UTF-8

ARG COMMIT_REF
WORKDIR /build/

# fetch `just` to manage build steps
RUN mkdir -p emsdk popcorn atomvm atomvm-out docs
RUN apt update && \
    apt -y install git curl && \
    curl -sSf https://just.systems/install.sh | bash -s -- --to /usr/local/bin

RUN cat <<EOF > justfile
export ERL_AFLAGS := '+JMsingle true'
export DEBIAN_FRONTEND := 'noninteractive'
export PATH := "/build/emsdk:/build/emsdk/upstream/emscripten:${HOME}/.local/share/mise/shims:${PATH}"
export MISE_TRUSTED_CONFIG_PATHS := '/build/popcorn'
export EMSDK_QUIET := '1'

all: deps atomvm artifacts
deps: _fetch_repos _system_deps _languages _emsdk
artifacts: docs example_hello_popcorn example_eval example_game_of_life example_iex

[group('dependencies')]
[working-directory('/build')]
_fetch_repos:
    git clone https://github.com/emscripten-core/emsdk.git /build/emsdk
    git clone https://github.com/software-mansion/popcorn.git /build/popcorn
    git clone https://github.com/software-mansion-labs/FissionVM.git /build/atomvm
    cd /build/popcorn && git fetch && git checkout "${COMMIT_REF}"
    cd /build/atomvm && git fetch && git checkout swm
    # prepare for cmake
    cd /build/atomvm/src/platforms/emscripten && mkdir -p build

[group('dependencies')]
_system_deps:
    apt update
    apt install -y cmake gperf libmbedtls-dev zlib1g-dev git \
    automake make gcc g++ libssl-dev libncurses-dev \
    python3 xz-utils \
    gpg wget curl

    # install mise
    install -dm 755 /etc/apt/keyrings
    wget -qO - https://mise.jdx.dev/gpg-key.pub | gpg --dearmor > /etc/apt/keyrings/mise-archive-keyring.gpg
    echo "deb [signed-by=/etc/apt/keyrings/mise-archive-keyring.gpg arch=amd64,arm64] https://mise.jdx.dev/deb stable main" > /etc/apt/sources.list.d/mise.list
    apt update
    apt install -y mise

[group('dependencies')]
_languages: _system_deps
    mise use --global node@"${NODE_VERSION}"
    mise use --global erlang@"${ERLANG_VERSION}"
    mise use --global elixir@"${ELIXIR_VERSION}"
    mise install
    mix local.rebar --force
    mix local.hex -if-missing --force

[group('dependencies')]
[working-directory('/build/emsdk')]
_emsdk: _fetch_repos _system_deps
    git pull
    ./emsdk install "${EMSDK_VERSION}"
    ./emsdk activate "${EMSDK_VERSION}"

[working-directory('/build/atomvm/src/platforms/emscripten/build')]
atomvm: deps
    emcmake cmake .. -DAVM_EMSCRIPTEN_ENV=web
    emmake make -j$(nproc)

    cp src/AtomVM.mjs src/AtomVM.wasm /build/atomvm-out/

[group('examples')]
example_hello_popcorn: (_example '/build/popcorn/examples/hello_popcorn')

[group('examples')]
example_eval: (_example '/build/popcorn/examples/eval_in_wasm')

[group('examples')]
example_game_of_life: (_example '/build/popcorn/examples/game_of_life')

[group('examples')]
[working-directory('/build/popcorn/examples/iex_wasm')]
example_iex: atomvm
    mkdir -p static/assets
    npm install --prefix ./static/assets @xterm/xterm
    mix deps.get
    mix popcorn.cook
    cp /build/atomvm-out/AtomVM.mjs static/wasm/
    cp /build/atomvm-out/AtomVM.wasm static/wasm/

[group('examples')]
_example dir: atomvm
    #!/usr/bin/env bash
    cd {{dir}} && \
    mix deps.get && \
    mix popcorn.cook && \
    cp /build/atomvm-out/AtomVM.mjs static/wasm/ && \
    cp /build/atomvm-out/AtomVM.wasm static/wasm/

[working-directory('/build/popcorn/misc/landing-page')]
docs: example_iex
    npm install
    npm run build
    # TODO: remove below copy (we popcorn.cook inside astro script which overwrites copied .wasm files in iex_wasm)
    cp /build/atomvm-out/AtomVM.mjs dist/wasm/
    cp /build/atomvm-out/AtomVM.wasm dist/wasm/
    cp -r dist/* /build/docs
EOF

RUN just docs

FROM nginx:alpine AS runtime

COPY --from=build /build/popcorn/misc/landing.nginx.conf /etc/nginx/nginx.conf
COPY --from=build /build/docs /usr/share/nginx/html
EXPOSE 8080
