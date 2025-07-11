FROM ubuntu:noble AS build

SHELL ["/bin/bash", "-c"]

ENV ERLANG_VERSION="26.0.2"
ENV ELIXIR_VERSION="1.17.3-otp-26"
ENV MIX_ENV=prod
ENV LC_ALL=C.UTF-8

WORKDIR /build/popcorn
COPY . .

# base deps
RUN touch /build/.env && \
    apt update && \
    apt -y install git curl && \
    curl --proto '=https' --tlsv1.2 -sSf https://just.systems/install.sh | bash -s -- --to /usr/local/bin

RUN cat <<EOF > justfile
# run .env script before running a command
set shell := ["/usr/bin/bash", "-c"]
export ERL_AFLAGS := '+JMsingle true'
export DEBIAN_FRONTEND := 'noninteractive'
export PATH := "/build/emsdk:/build/emsdk/upstream/emscripten:${HOME}/.local/share/mise/shims:${PATH}"
export MISE_TRUSTED_CONFIG_PATHS := '/build/popcorn'
export EMSDK_QUIET := '1'

all: deps atomvm artifacts
deps: _system_deps _emsdk
artifacts: docs example_hello_popcorn example_eval example_game_of_life example_iex

[group('dependencies')]
[working-directory('/build')]
_dirs:
    cd popcorn && git clean -dfx --exclude='justfile'
    rm -rf /build/atomvm
    rm -rf /build/emsdk
    rm -rf /build/atomvm-out
    rm -rf /build/docs

    mkdir -p emsdk atomvm atomvm-out docs

[group('dependencies')]
_system_deps: _dirs
    #!/usr/bin/env bash
    set -euo pipefail

    if command -v elixir >/dev/null 2>&1; then
        echo "Elixir already installed, skipping dependencies installation..."
        exit 0
    fi

    apt update
    apt install -y cmake gperf libmbedtls-dev zlib1g-dev git \
    automake make gcc g++ libssl-dev libncurses-dev \
    python3 xz-utils \
    gpg wget curl

    # install mise
    install -dm 755 /etc/apt/keyrings
    wget -qO - https://mise.jdx.dev/gpg-key.pub | gpg --dearmor > /etc/apt/keyrings/mise-archive-keyring.gpg
    echo "deb [signed-by=/etc/apt/keyrings/mise-archive-keyring.gpg arch=amd64] https://mise.jdx.dev/deb stable main" > /etc/apt/sources.list.d/mise.list
    apt update
    apt install -y mise

    # add elixir, erlang, node (and tools)
    mise use --global node@22
    mise install
    mix local.rebar --force
    mix local.hex -if-missing --force

[group('dependencies')]
[working-directory('/build/emsdk')]
_emsdk: _dirs _system_deps
    #!/usr/bin/env bash
    set -euo pipefail

    if command -v emmake >/dev/null 2>&1; then
        echo "Emsdk already installed, skipping..."
        exit 0
    fi

    git clone https://github.com/emscripten-core/emsdk.git .
    git pull
    ./emsdk install 4.0.8
    ./emsdk activate 4.0.8


[working-directory('/build/atomvm')]
atomvm: deps
    #!/usr/bin/env bash
    set -euo pipefail

    if [[ -d ".git" ]]; then
        echo "FissionVM already cloned, skipping clone..."
    else
        git clone https://github.com/software-mansion-labs/FissionVM.git .
    fi

    git pull
    mkdir -p src/platforms/emscripten/build
    cd src/platforms/emscripten/build
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

    cp -r /build/atomvm-out/* static/wasm

[group('examples')]
_example dir: atomvm
    cd {{dir}} && \
    mix deps.get && \
    mix popcorn.cook && \
    cp -r /build/atomvm-out/* static/wasm

[working-directory('/build/popcorn/misc/landing-page')]
docs: example_iex
    npm install
    npm run build
    # TODO: remove below copy (we popcorn.cook inside astro script which overwrites copied .wasm files in iex_wasm)
    cp -r /build/atomvm-out/* dist/wasm
    cp -r dist/* /build/docs
EOF

RUN just artifacts

FROM nginx:alpine AS runtime

COPY --from=build /build/popcorn/misc/landing.nginx.conf /etc/nginx/nginx.conf
COPY --from=build /build/docs /usr/share/nginx/html
EXPOSE 8080
