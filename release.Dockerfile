FROM ubuntu:noble

ENV ERLANG_VERSION="26.0.2"
ENV ELIXIR_VERSION="1.17.3-otp-26"
ENV NODE_VERSION="22"
ENV EMSDK_VERSION="4.0.8"
ENV MIX_ENV=prod
ENV LC_ALL=C.UTF-8

ENV ERL_AFLAGS='+JMsingle true'
ENV DEBIAN_FRONTEND='noninteractive'
# need for reduced job count when building AtomVM
ENV CI=true
ENV PATH="/build/emsdk:/build/emsdk/upstream/emscripten:/root/.local/share/mise/shims:$PATH"
ENV EMSDK_QUIET='1'

WORKDIR /build/

RUN apt-get -y update && \
    apt-get -y install git curl cmake gperf libmbedtls-dev zlib1g-dev \
    automake make gcc g++ libssl-dev libncurses-dev \
    python3 xz-utils gpg wget ninja-build

RUN install -dm 755 /etc/apt/keyrings && \
    wget -qO - https://mise.jdx.dev/gpg-key.pub | gpg --dearmor > /etc/apt/keyrings/mise-archive-keyring.gpg && \
    echo "deb [signed-by=/etc/apt/keyrings/mise-archive-keyring.gpg arch=amd64,arm64] https://mise.jdx.dev/deb stable main" > /etc/apt/sources.list.d/mise.list && \
    apt update && \
    apt install -y mise

RUN mise use --global node@"${NODE_VERSION}" && mise install
RUN mise use --global pnpm@10 && mise install

RUN mise use --global erlang@"${ERLANG_VERSION}" && mise install
RUN mise use --global elixir@"${ELIXIR_VERSION}" && \
    mise install && \
    mix local.rebar --force && \
    mix local.hex -if-missing --force

RUN mise use --global emsdk@"${EMSDK_VERSION}" && mise install

# Clone the repository
ARG GIT_REPO=https://github.com/software-mansion/popcorn.git
ARG GIT_REF
RUN git clone --depth 1 --branch "${GIT_REF}" "${GIT_REPO}" /build/popcorn && \
    mise trust /build/popcorn

# Set RUNTIME_SOURCE for production build
ENV RUNTIME_SOURCE='https://github.com/software-mansion-labs/FissionVM.git#swm'

WORKDIR /build/popcorn/js
RUN pnpm config set enable-pre-post-scripts true && \
    pnpm install --frozen-lockfile

RUN pnpm -F popcorn build:prod
RUN pnpm -F popcorn pack

RUN mkdir -p /output && \
    cp -r /build/popcorn/js/popcorn/dist /output/
