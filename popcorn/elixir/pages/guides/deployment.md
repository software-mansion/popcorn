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

## Caching

Most of the packager's output has a SHA-256 hash of its contents in its
filename, so a new version gets a new name. Browsers can cache these files for
good, and the few files that name them tell browsers which ones are current.

### Hashed files

- `otp/lib/stdlib.<hash>.tar`, `otp/lib/app.<hash>.tar` and
  `otp/lib/deps.<hash>.tar`: the archives of the selected Erlang/Elixir
  built-ins, your code, and your dependencies. Tree-shaking runs on all
  applications, so a change in your app may include or drop a module from a
  standard library and change its hash too.
- `beam.<hash>.wasm`: the VM.
- `beam.emu.<hash>.mjs`: the script that loads the VM. Each of the VM's threads
  loads it in a worker of its own, so unless it's cached, the browser
  revalidates it once for each thread, one request after another.

Serve them with:

```http
Cache-Control: public, max-age=31536000, immutable
```

Browsers then don't request them again until their names change.

### Other files

`otp/manifest.json` names the archives, and `worker.mjs` names the VM's script,
which names the VM. They and the rest of the output, like `index.mjs`, keep
their names, so serve them with:

```http
Cache-Control: no-cache
```

It makes browsers always verify that their cache is up-to-date.

The [sample nginx config](#sample-nginx-config-for-hosting-popcorn-applications)
below sets both headers.

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
        "~\.[a-f0-9]{64}\.(tar|mjs|wasm)$" "public, max-age=31536000, immutable";
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
