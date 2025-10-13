FROM ghcr.io/software-mansion/popcorn-base:latest AS build_lang_tour

ARG SENTRY_DSN
ARG SENTRY_MODE

WORKDIR /build/popcorn/misc/language-tour-guide/elixir_tour
RUN mix deps.get
RUN mix popcorn.cook

WORKDIR /build/popcorn/misc/language-tour-guide
RUN npm install
RUN VITE_SENTRY_DSN="${SENTRY_DSN}" VITE_MODE="${SENTRY_MODE}" npm run build
RUN cp -r dist/* /build/out

FROM nginx:alpine AS runtime
COPY --from=build_lang_tour  /build/popcorn/misc/langtour.nginx.conf /etc/nginx/nginx.conf
COPY --from=build_lang_tour  /build/out /usr/share/nginx/html
EXPOSE 8080
