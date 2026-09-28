import "phoenix_html";
import { Socket } from "phoenix";
import { LiveSocket } from "phoenix_live_view";
import { LLVEngine } from "local_live_view";

const csrfToken = document.querySelector("meta[name='csrf-token']").getAttribute("content");
const liveSocket = new LiveSocket("/live", Socket, { params: { _csrf_token: csrfToken } });

// LLVEngine.create must come before liveSocket.connect()
const llvEngine = LLVEngine.create(liveSocket, { bundlePaths: ["/assets/js/wasm/bundle.avm"] });
liveSocket.connect();
llvEngine.connect();

// The tests inspect the socket
window.liveSocket = liveSocket;
