FROM ghcr.io/software-mansion/popcorn-base:latest AS build_landing

WORKDIR /build/popcorn/examples/iex_wasm
RUN mix deps.get
RUN mix popcorn.cook

WORKDIR /build/popcorn/misc/landing-page
RUN npm install
RUN npm run build
RUN cp -r dist/* /build/out

FROM nginx:alpine AS runtime
COPY --from=build_landing  /build/popcorn/misc/landing.nginx.conf /etc/nginx/nginx.conf
COPY --from=build_landing  /build/out /usr/share/nginx/html
EXPOSE 8080
