FROM ubuntu:noble

ENV ERLANG_VERSION="29.0.6"
ENV ELIXIR_VERSION="1.20.4-otp-29"
ENV NODE_VERSION="22"
ENV EMSDK_VERSION="4.0.8"
ENV MIX_ENV=prod
ENV LC_ALL=C.UTF-8
ENV CI=true

ENV DEBIAN_FRONTEND='noninteractive'
ENV PATH="/build/emsdk:/build/emsdk/upstream/emscripten:/root/.local/share/mise/shims:$PATH"
ENV EMSDK_QUIET='1'

ARG COMMIT_REF

WORKDIR /build/
RUN mkdir -p popcorn out
RUN apt-get -y update && \
    apt-get -y install git curl cmake gperf libmbedtls-dev zlib1g-dev \
    automake make gcc g++ libssl-dev libncurses-dev \
    default-jre-headless python3 xz-utils gpg wget

RUN wget -q https://ftp.gnu.org/gnu/autoconf/autoconf-2.72.tar.gz && \
    tar xzf autoconf-2.72.tar.gz && \
    cd autoconf-2.72 && \
    ./configure --prefix=/usr/local && \
    make -j1 && \
    make install

# install mise
RUN install -dm 755 /etc/apt/keyrings && \
    wget -qO - https://mise.jdx.dev/gpg-key.pub | gpg --dearmor > /etc/apt/keyrings/mise-archive-keyring.gpg && \
    echo "deb [signed-by=/etc/apt/keyrings/mise-archive-keyring.gpg arch=amd64,arm64] https://mise.jdx.dev/deb stable main" > /etc/apt/sources.list.d/mise.list && \
    apt update && \
    apt install -y mise

RUN mise use --global node@"${NODE_VERSION}" && mise install
RUN mise use --global pnpm@11 && mise install
RUN mise use --global erlang@"${ERLANG_VERSION}" && mise install
RUN mise use --global elixir@"${ELIXIR_VERSION}" && \
    mise install && \
    mix local.rebar --force && \
    mix local.hex -if-missing --force
RUN mise install erlang@26.0.2 elixir@1.17.3-otp-26 && \
    mise exec erlang@26.0.2 elixir@1.17.3-otp-26 -- mix local.rebar --force && \
    mise exec erlang@26.0.2 elixir@1.17.3-otp-26 -- mix local.hex --force
RUN mise use --global emsdk@"${EMSDK_VERSION}" && mise install

RUN git clone https://github.com/software-mansion/popcorn.git /build/popcorn && \
    mise trust /build/popcorn
RUN cd /build/popcorn && git fetch && git checkout "${COMMIT_REF}"

# Install JS workspace dependencies
WORKDIR /build/popcorn
RUN pnpm install --frozen-lockfile --child-concurrency=1 --network-concurrency=1

# Build the OTP/BEAM runtime and canonical JavaScript package.
RUN git config --global user.email "ci@ci.local" && \
    git config --global user.name "CI"
RUN mise exec erlang@"${ERLANG_VERSION}" elixir@"${ELIXIR_VERSION}" -- \
    scripts/build-beam.sh release
RUN mise exec erlang@"${ERLANG_VERSION}" elixir@"${ELIXIR_VERSION}" -- \
    pnpm -F ./popcorn/js build
