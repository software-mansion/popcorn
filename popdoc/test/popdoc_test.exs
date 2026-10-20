defmodule PopdocTest do
  use ExUnit.Case

  test "config/1 adds popdoc assets and scripts" do
    opts = Popdoc.config(main: "readme")

    assert opts[:markdown_processor] == Popdoc.Markdown
    [{bundle_root, "assets"}] = Map.to_list(opts[:assets])
    assert File.exists?(Path.join(bundle_root, "worker.mjs"))
    assert File.exists?(Path.join(bundle_root, "otp/manifest.json"))
    assert opts[:before_closing_head_tag].(:html) =~ "assets/popdoc.css"
    assert opts[:before_closing_body_tag].(:html) =~ "assets/popdoc.js"
  end

  test "config/1 adds emulation for CORS/COEP headers if requested" do
    opts = Popdoc.config(main: "readme", popdoc: [coi_serviceworker: true])

    assert Map.has_key?(opts[:assets], Path.expand("../docs_assets", __DIR__))
    assert opts[:before_closing_head_tag].(:html) =~ "coi-serviceworker.js"
  end
end
