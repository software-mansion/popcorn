ARG POPCORN_BASE_IMAGE=ghcr.io/software-mansion/popcorn-base
ARG POPCORN_BASE_TAG=latest
FROM ${POPCORN_BASE_IMAGE}:${POPCORN_BASE_TAG} AS build_landing

WORKDIR /build/popcorn/landing-page
RUN pnpm run build
RUN cp -r dist/* /build/out

FROM nginx:alpine AS runtime
COPY --from=build_landing  /build/popcorn/docker/landing.nginx.conf /etc/nginx/nginx.conf
COPY --from=build_landing  /build/out /usr/share/nginx/html
EXPOSE 8080
