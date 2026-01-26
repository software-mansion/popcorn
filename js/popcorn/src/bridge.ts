import type {
  AnySerializable,
  IframeRequest,
  IframeResponse,
} from "./types";

export type { AnySerializable, IframeRequest, IframeResponse };

/** Union of all messages (requests and responses) */
export type Message = IframeRequest | IframeResponse;

export type IframeBridgeArgs<T> = {
  container: HTMLElement;
  config: Record<string, string>;
  script: { url: string; entrypoint: string };
  debug: boolean;
  messageFilter: (data: unknown) => data is T;
  onMessage: (data: T) => void;
};

const STYLE_HIDDEN =
  "visibility: hidden; width: 0px; height: 0px; border: none";

export const MESSAGES = {
  INIT: "popcorn-init",
  START_VM: "popcorn-startVm",
  CALL: "popcorn-call",
  CAST: "popcorn-cast",
  CALL_ACK: "popcorn-callAck",
  STDOUT: "popcorn-stdout",
  STDERR: "popcorn-stderr",
  HEARTBEAT: "popcorn-heartbeat",
  RELOAD: "popcorn-reload",
} as const;

const MESSAGES_TYPES = new Set<string>(Object.values(MESSAGES));
export function isMessageType(type: string): type is Message["type"] {
  return MESSAGES_TYPES.has(type);
}

export function send(type: string, data: any): void {
  window.parent.postMessage({ type, value: data });
}

export class IframeBridge<T> {
  private iframe: HTMLIFrameElement;
  private handlerRef: (event: MessageEvent) => void;
  // @ts-expect-error TODO: use for tracing
  private debug: boolean;
  private messageFilter: (data: unknown) => data is T;
  private onMessage: (data: T) => void;

  public constructor(args: IframeBridgeArgs<T>) {
    const { container, config, script, debug, messageFilter, onMessage } = args;
    this.debug = debug;
    this.messageFilter = messageFilter;
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

  public send<T>(data: T): void {
    this.iframe?.contentWindow?.postMessage(data);
  }

  public deinit() {
    window.removeEventListener("message", this.handlerRef);
    this.iframe.remove();
  }

  private messageHandler({ data }: MessageEvent): void {
    if (this.messageFilter(data)) {
      this.onMessage(data);
    }
  }
}

function metaTagsFrom(config: Record<string, string>) {
  return Object.entries(config)
    .map(([key, value]) => `<meta name="${key}" content="${value}" />`)
    .join("\n");
}
