ARG POPCORN_BASE_IMAGE=ghcr.io/software-mansion/popcorn-base
ARG POPCORN_BASE_TAG=latest
FROM ${POPCORN_BASE_IMAGE}:${POPCORN_BASE_TAG} AS build_lang_tour

ARG SYNC_THIRD_PARTY_ASSETS=false
ARG SENTRY_DSN
ARG SENTRY_MODE
ARG APP_VERSION

WORKDIR /build/popcorn/language-tour
# Keep in sync with docker/landing.Dockerfile.
RUN if [ "$SYNC_THIRD_PARTY_ASSETS" = "true" ]; then \
      curl -fsSL "https://cdn.cookie-script.com/s/19b5b47f7a8f2606f861864571339358.js" -o public/cookie-script.js; \
    fi
RUN VITE_SENTRY_DSN="${SENTRY_DSN}" VITE_MODE="${SENTRY_MODE}" VITE_APP_VERSION="${APP_VERSION}" pnpm run build
RUN cp -r dist/* /build/out

FROM nginx:alpine AS runtime
COPY --from=build_lang_tour  /build/popcorn/docker/langtour.nginx.conf /etc/nginx/nginx.conf
COPY --from=build_lang_tour  /build/out /usr/share/nginx/html
EXPOSE 8080
