ARG POPCORN_BASE_IMAGE=ghcr.io/software-mansion/popcorn-base
ARG POPCORN_BASE_TAG=latest
FROM ${POPCORN_BASE_IMAGE}:${POPCORN_BASE_TAG} AS build_landing

ARG SYNC_THIRD_PARTY_ASSETS=false

WORKDIR /build/popcorn/landing-page
# Keep in sync with docker/langtour.Dockerfile.
RUN if [ "$SYNC_THIRD_PARTY_ASSETS" = "true" ]; then \
      curl -fsSL "https://cdn.cookie-script.com/s/19b5b47f7a8f2606f861864571339358.js" -o public/cookie-script.js; \
    fi
RUN pnpm run build
RUN cp -r dist/* /build/out

FROM nginx:alpine AS runtime
COPY --from=build_landing  /build/popcorn/docker/landing.nginx.conf /etc/nginx/nginx.conf
COPY --from=build_landing  /build/out /usr/share/nginx/html
EXPOSE 8080
