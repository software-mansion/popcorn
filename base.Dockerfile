FROM ubuntu:noble

ENV ERLANG_VERSION="26.0.2"
ENV ELIXIR_VERSION="1.17.3-otp-26"
ENV NODE_VERSION="22"
ENV EMSDK_VERSION="4.0.8"
ENV MIX_ENV=prod
ENV LC_ALL=C.UTF-8

ARG COMMIT_REF

ENV ERL_AFLAGS='+JMsingle true'
ENV DEBIAN_FRONTEND='noninteractive'
ENV PATH="/build/emsdk:/build/emsdk/upstream/emscripten:/root/.local/share/mise/shims:$PATH"
ENV MISE_TRUSTED_CONFIG_PATHS='/build/popcorn'
ENV EMSDK_QUIET='1'


WORKDIR /build/
RUN mkdir -p popcorn atomvm out
RUN apt-get -y update && \
    apt-get -y install git curl cmake gperf libmbedtls-dev zlib1g-dev git \
    automake make gcc g++ libssl-dev libncurses-dev \
    python3 xz-utils gpg wget

# install mise
RUN install -dm 755 /etc/apt/keyrings && \
    wget -qO - https://mise.jdx.dev/gpg-key.pub | gpg --dearmor > /etc/apt/keyrings/mise-archive-keyring.gpg && \
    echo "deb [signed-by=/etc/apt/keyrings/mise-archive-keyring.gpg arch=amd64,arm64] https://mise.jdx.dev/deb stable main" > /etc/apt/sources.list.d/mise.list && \
    apt update && \
    apt install -y mise

RUN mise use --global node@"${NODE_VERSION}" && mise install
RUN mise use --global erlang@"${ERLANG_VERSION}" && mise install
RUN mise use --global elixir@"${ELIXIR_VERSION}" && \
    mise install && \
    mix local.rebar --force && \
    mix local.hex -if-missing --force
RUN mise use --global emsdk@"${EMSDK_VERSION}" && mise install

RUN git clone https://github.com/software-mansion-labs/FissionVM.git --branch=swm --depth=1 /build/atomvm
RUN cd /build/atomvm/src/platforms/emscripten && mkdir -p build
RUN git clone https://github.com/software-mansion/popcorn.git /build/popcorn
RUN cd /build/popcorn && git fetch && git checkout "${COMMIT_REF}"

# Build AtomVM WASM
WORKDIR /build/atomvm/src/platforms/emscripten/build
RUN emcmake cmake .. -DAVM_EMSCRIPTEN_ENV=web
RUN emmake make -j$(nproc)
RUN cp src/AtomVM.mjs src/AtomVM.wasm /build/out/
