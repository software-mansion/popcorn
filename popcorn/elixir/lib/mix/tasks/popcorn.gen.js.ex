defmodule Mix.Tasks.Popcorn.Gen.Js do
  @shortdoc "Scaffolds JS asset files for a Popcorn project."
  @moduledoc "#{@shortdoc}"
  use Mix.Task
  import Mix.Generator

  @popcorn_version Mix.Project.config()[:version]

  @impl true
  def run(_args) do
    app_name =
      Mix.Project.config()[:app]
      |> to_string()
      |> String.replace("_", "-")

    create_file("assets/package.json", package_json_template(app_name: app_name))
    create_file("assets/build.mjs", build_mjs_text())
    create_file("assets/index.js", index_js_text())
    create_file("assets/index.html", index_html_text())

    Mix.shell().info("""

    Next steps:
      npm install --prefix assets
      mix popcorn.cook
      npm run build --prefix assets
      mix popcorn.server
    """)
  end

  embed_template(:package_json, """
  {
    "name": "<%= @app_name %>",
    "private": true,
    "scripts": {
      "build": "node build.mjs"
    },
    "dependencies": {
      "@swmansion/popcorn": "^#{@popcorn_version}"
    },
    "devDependencies": {
      "esbuild": "^0.25.0"
    }
  }
  """)

  embed_text(:build_mjs, """
  import * as esbuild from "esbuild";
  import { popcorn } from "@swmansion/popcorn/esbuild";
  import { copyFile, mkdir } from "fs/promises";

  await mkdir("../dist", { recursive: true });
  await copyFile("index.html", "../dist/index.html");

  await esbuild.build({
    entryPoints: ["index.js"],
    bundle: true,
    format: "esm",
    sourcemap: true,
    outfile: "../dist/index.js",
    plugins: [popcorn({ bundlePaths: ["../dist/wasm/bundle.avm"] })],
  });
  """)

  embed_text(:index_js, """
  import { Popcorn } from "@swmansion/popcorn";

  await Popcorn.init({ bundlePaths: ["/wasm/bundle.avm"], onStdout: console.log });
  """)

  embed_text(:index_html, """
  <html>
    <script type="module" src="./index.js"></script>
    <body></body>
  </html>
  """)
end
