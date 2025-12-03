import { log } from "./utils";
import iframeBundleString from "virtual:iframe-bundle";

export interface PopcornConfig {
  containerId?: string;
  onReady?: () => void;
  bundlePath?: string;
}

export interface PopcornInstance {
  deinit: () => void;
}

export async function init(
  config: PopcornConfig = {},
): Promise<PopcornInstance> {
  log("Initializing Popcorn");

  // Create hidden iframe
  const iframe = document.createElement("iframe");
  iframe.style.display = "none";
  iframe.style.width = "0";
  iframe.style.height = "0";
  iframe.style.border = "none";

  const bundlePath = config.bundlePath || "/bundle.avm";

  // Use srcdoc to pass the bundled iframe code with bundle path in meta tag
  iframe.srcdoc = `
    <!DOCTYPE html>
    <html>
      <head>
        <meta charset="utf-8">
        <meta name="bundle-path" content="${bundlePath}">
      </head>
      <body>
        <script type="module">
          ${iframeBundleString}
        </script>
      </body>
    </html>
  `;

  // Append to body
  document.body.appendChild(iframe);

  log("Popcorn initialized with hidden iframe");

  if (config.onReady) {
    config.onReady();
  }

  // Return instance with cleanup function
  return {
    deinit: () => {
      log("Deinitializing Popcorn");

      if (iframe && iframe.parentNode) {
        iframe.parentNode.removeChild(iframe);
      }

      log("Popcorn deinitialized");
    },
  };
}
