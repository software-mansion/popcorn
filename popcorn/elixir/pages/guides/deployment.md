# Deployment

To host a Popcorn app, you need to configure your web server.

## Set cross-origin isolation headers

Popcorn requires [`SharedArrayBuffer`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/SharedArrayBuffer) to work. This buffer will work on the page if you send the following headers with the page response:

```text
Cross-Origin-Opener-Policy: same-origin
Cross-Origin-Embedder-Policy: require-corp
```

In development, both `mix popcorn.dev` and the Vite plugin set them.

## Set response metadata

1. Serve `.wasm` files with `Content-Type: application/wasm`.
2. Set `Content-Encoding: gzip` or `Content-Encoding: br` to serve compressed assets. Popcorn generates them automatically. `br` is Brotli compression and works better than Gzip. Compression level is configurable in the packager. Set `Vary: Accept-Encoding` when selecting a representation.

## Cache bundles independently

The packager emits three archives: `stdlib.<hash>.tar` for selected Erlang/Elixir
built-ins, `app.<hash>.tar` for user code, and `deps.<hash>.tar` for dependencies.

Each filename contains the SHA-256 of its final uncompressed contents. The boot
script also has a content hash in its filename.
Tree-shaking runs on all applications which means a change in your app may include or drop a module from a standard library and change the hash.

Serve hashed tarballs with:

```http
Cache-Control: public, max-age=31536000, immutable
```

This prevent re-requesting them if they didn't change. They're referenced by `manifest.json` which should be checked periodically for using new hashed tarballs.

## Avoid transforming the output

Popcorn relies on Web Workers, which need stable paths for the scripts they execute. Popcorn's packager is configured to output the correct file layout. Keep that layout when deploying.

Use `beam.otpAssetsRoot` if you want to run it on subpage. The value must end with `/`.

## Configure the Content Security Policy

`Popcorn.JS.run/3` currently evaluates JavaScript source. The page Content
Security Policy must permit `unsafe-eval`. Popcorn initialization will fail if it's not allowed.

## Sample nginx config for hosting Popcorn applications

This example is based on the nginx configuration used for Popcorn's landing page.
Save it as `/etc/nginx/nginx.conf`, replace `example.com` with your domain, and copy
your application's generated files to `/usr/share/nginx/html`.

It listens on HTTP port 80. Use it behind a proxy that terminates HTTPS, or configure
TLS in nginx. Browsers require HTTPS for cross-origin isolation, except on localhost.

```nginx
worker_processes auto;

events {
    worker_connections 1024;
}

http {
    include /etc/nginx/mime.types;
    default_type application/octet-stream;

    map $uri $popcorn_cache_control {
        default "no-cache";
        "~\.[a-f0-9]{64}\.(tar|boot)$" "public, max-age=31536000, immutable";
    }

    server {
        listen 80;
        listen [::]:80;
        server_name example.com;
        port_in_redirect off;

        root /usr/share/nginx/html;
        index index.html;

        add_header Cross-Origin-Opener-Policy "same-origin" always;
        add_header Cross-Origin-Embedder-Policy "require-corp" always;
        add_header Cross-Origin-Resource-Policy "cross-origin" always;

        add_header Cache-Control $popcorn_cache_control;

        gzip_static on;
        gzip_vary on;
        gzip_proxied any;

        location ~ \.(wasm|mjs|tar|boot|json)$ {
            types {
                application/wasm wasm;
                application/javascript mjs;
                application/json json;
            }
            try_files $uri =404;
        }

        location / {
            try_files $uri $uri/ =404;
        }
    }
}
```

[`gzip_static`](https://nginx.org/en/docs/http/ngx_http_gzip_static_module.html)
serves generated `.gz` files when the client accepts gzip and sets `Content-Encoding`
automatically. Your nginx build must include `ngx_http_gzip_static_module`.
This example uses gzip; serving `.br` files requires an additional Brotli module.

If your application uses client-side routing, replace `try_files $uri $uri/ =404;`
with `try_files $uri $uri/ /index.html;`. Keep the asset location unchanged so missing
runtime files, manifests, and archives return 404.

Validate the configuration with `nginx -t` before reloading nginx.
