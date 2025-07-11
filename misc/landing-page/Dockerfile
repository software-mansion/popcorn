# ------------------------ Build base image with Elixir, Erlang, Emscripten and AtomVM in WASM
FROM ubuntu:noble AS statics

SHELL ["/bin/bash", "-c"]

ENV ERLANG_VERSION="26.0.2"
ENV ELIXIR_VERSION="1.17.3-otp-26"
ENV MIX_ENV=prod
ENV LC_ALL=C.UTF-8

## replace sh with bash
# TODO: possibly not needed if using Docker instead of Podman that uses OCI images
# RUN ln -snf /bin/bash /bin/sh

# deps for AtomVM, mise, Erlang, emscripten, and fixing locale
RUN apt update && \
    apt install -y cmake gperf libmbedtls-dev zlib1g-dev git \
                   gpg wget curl \
                   automake make gcc g++ libssl-dev libncurses-dev \
                   python3 xz-utils

# install mise
RUN install -dm 755 /etc/apt/keyrings && \
    wget -qO - https://mise.jdx.dev/gpg-key.pub | gpg --dearmor > /etc/apt/keyrings/mise-archive-keyring.gpg && \
    echo "deb [signed-by=/etc/apt/keyrings/mise-archive-keyring.gpg arch=amd64] https://mise.jdx.dev/deb stable main" > /etc/apt/sources.list.d/mise.list && \
    apt update && apt install -y mise

# install Erlang & Elixir
RUN mise use --global erlang@"$ERLANG_VERSION" && \
    mise use --global elixir@"$ELIXIR_VERSION" && \
    mise env >> /root/.env

# install rebar and hex
RUN source /root/.env && \
  mix local.rebar --force && \
  mix local.hex -if-missing --force

# install emscripten
RUN git clone https://github.com/emscripten-core/emsdk.git /root/emsdk && \
    cd /root/emsdk && git pull && \
    ./emsdk install 4.0.8 && \
    ./emsdk activate 4.0.8 && \
    echo 'source "/root/emsdk/emsdk_env.sh"' >> /root/.env

# make AtomVM WASM
RUN git clone https://github.com/software-mansion-labs/FissionVM.git /root/FissionVM
RUN source /root/.env && \
    mkdir -p /root/wasm && \
    cd /root/FissionVM/src/platforms/emscripten && \
    mkdir build && cd build && \
    emcmake cmake .. -DAVM_EMSCRIPTEN_ENV=web && \
    emmake make -j${nproc} && \
    cp src/AtomVM.mjs src/AtomVM.wasm /root/wasm/

ARG COMMIT_REF ''
RUN git clone https://github.com/software-mansion/popcorn.git /root/popcorn && \
    git clone https://github.com/software-mansion-labs/elixir-docs-wasm.git /root/elixir && \
    cd /root/elixir && git checkout jgonet/docs
RUN cd /root/popcorn && \
    git fetch && git checkout "$COMMIT_REF" && \
    cp -r /root/popcorn/priv/static-template/wasm /root/

 #------------------------ Build static files
 RUN echo 'export ERL_AFLAGS="+JMsingle true"' >> /root/.env
 RUN source /root/.env && \
     cd /root/popcorn/examples/eval_in_wasm && \
     mix deps.get && mix popcorn.cook && \
     cp -r /root/wasm static

 RUN source /root/.env && \
     cd /root/popcorn/examples/game_of_life && \
     mix deps.get && mix popcorn.cook && \
     cp -r /root/wasm static

 RUN source /root/.env && \
     cd /root/popcorn/examples/iex_wasm && \
     mkdir static/assets && \
     npm install --prefix ./static/assets @xterm/xterm && \
     mix deps.get && mix popcorn.cook && \
     cp -r /root/wasm static

 RUN source /root/.env && \
     cd /root/popcorn/ && \
     cp -r examples/eval_in_wasm/static/ utils/landing-page/simple_repl && \
     cp -r examples/game_of_life/static/ utils/landing-page/game_of_life && \
     cp -r examples/iex_wasm/static/ utils/landing-page/iex_wasm && \
     cp -r /root/elixir/generated-docs/ utils/landing-page/elixir_docs && \
     cp -r utils/landing-page /landing-page

# ------------------------ Host static files
FROM nginx:1.27.5-alpine-slim
COPY --from=statics /landing-page /var/www/html
COPY default.conf.template /etc/nginx/conf.d/default.conf
