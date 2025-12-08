import { StrictMode } from "react";
import { createRoot } from "react-dom/client";
import { BrowserRouter } from "react-router";

import { createLogSink, initSentry } from "./utils/sentry";
import { syncContentHash } from "./utils/storage";

import "./styles/index.css";
import { AppRoutes } from "./AppRoutes.tsx";
import { PopcornProvider } from "./context/popcorn/PopcornProvider.tsx";

syncContentHash();

const container = document.getElementById("root");
if (container === null) {
  throw new Error("Root not found");
}

initSentry();

createRoot(container).render(
  <PopcornProvider debug={true} logSink={createLogSink()}>
    <StrictMode>
      <BrowserRouter basename={import.meta.env.BASE_URL}>
        <AppRoutes />
      </BrowserRouter>
    </StrictMode>
  </PopcornProvider>
);
