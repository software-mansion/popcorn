import {
  type IframeRequest,
  type IframeResponse,
  type AnySerializable,
  isMessageType,
} from "./types";

export type IframeBridgeArgs = {
  container: HTMLElement;
  config: Record<string, string>;
  script: { url: string; entrypoint: string };
  debug: boolean;
  onMessage: (data: IframeResponse) => void;
};

const STYLE_HIDDEN =
  "visibility: hidden; width: 0px; height: 0px; border: none";

export function send(type: string, data: AnySerializable): void {
  window.parent.postMessage({ type, value: data });
}

export class IframeBridge {
  private iframe: HTMLIFrameElement;
  private handlerRef: (event: MessageEvent) => void;
  // @ts-expect-error TODO: use for tracing
  private debug: boolean;
  private onMessage: (data: IframeResponse) => void;

  public constructor(args: IframeBridgeArgs) {
    const { container, config, script, debug, onMessage } = args;
    this.debug = debug;
    this.onMessage = onMessage;

    this.iframe = document.createElement("iframe");
    this.iframe.srcdoc = `<html>
      <html lang="en" dir="ltr">
          <head>
          ${metaTagsFrom(config)}
          </head>
          <script type="module" defer>
            import { ${script.entrypoint} } from "${script.url}";
            ${script.entrypoint}();
          </script>
      </html>`;
    this.iframe.style = STYLE_HIDDEN;

    this.handlerRef = this.messageHandler.bind(this);
    window.addEventListener("message", this.handlerRef);

    // mount
    container.appendChild(this.iframe);
  }

  public send(data: IframeRequest): void {
    this.iframe?.contentWindow?.postMessage(data);
  }

  public deinit() {
    window.removeEventListener("message", this.handlerRef);
    this.iframe.remove();
  }

  private messageHandler({ data }: MessageEvent): void {
    if (isIframeResponse(data)) {
      this.onMessage(data);
    }
  }
}

function isIframeResponse(payload: unknown): payload is IframeResponse {
  if (typeof payload !== "object" || payload === null) return false;
  if (!Object.hasOwn(payload, "type") || !Object.hasOwn(payload, "value"))
    return false;
  if (typeof (payload as { type: unknown }).type !== "string") return false;
  return isMessageType((payload as { type: string }).type);
}

function metaTagsFrom(config: Record<string, string>) {
  return Object.entries(config)
    .map(([key, value]) => `<meta name="${key}" content="${value}" />`)
    .join("\n");
}
