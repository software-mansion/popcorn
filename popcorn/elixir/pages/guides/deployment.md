# Deployment

To host the Popcorn app you need to configure your webserver.

## Set cross-origin isolation headers

Popcorn requires [`SharedArrayBuffer`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/SharedArrayBuffer) to work. This buffer will work on the page if you pass following headers with the page response:

```text
Cross-Origin-Opener-Policy: same-origin
Cross-Origin-Embedder-Policy: require-corp
```

In development, both `popcorn.dev` and Vite plugins sets them.

## Set response metadata

1. Serve `.wasm` file with `Content-type: application/wasm`.
2. Set `Content-Encoding: gzip` or `Content-Encoding: br` to serve compressed assets. Popcorn generates them automatically. `br` is Brotli compression and works better than Gzip. Compression is configurable in the packager.

## Avoid transforming the output

Popcorn relies on Web Workers which needs stable paths for scripts they will execute. Popcorn's packager is configured to output file layout correctly, make sure you don't foil its plans.

Use `beam.otpAssetsRoot` if you want to run it on subpage. The value must end with `/`.

## Configure the Content Security Policy

`Popcorn.JS.run_js/3` currently evaluates JavaScript source. The page Content
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

        gzip_static on;
        gzip_vary on;
        gzip_proxied any;

        location ~ \.(wasm|mjs)$ {
            types {
                application/wasm wasm;
                application/javascript mjs;
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
`.wasm` and `.mjs` files return 404.

Validate the configuration with `nginx -t` before reloading nginx.
