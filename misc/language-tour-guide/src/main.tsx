import { StrictMode } from "react";
import { createRoot } from "react-dom/client";
import { BrowserRouter } from "react-router";

import "./styles/index.css";
import { AppRoutes } from "./AppRoutes.tsx";
import { PopcornProvider } from "./context/popcorn/PopcornProvider.tsx";

const container = document.getElementById("root");
if (container === null) {
  throw new Error("Root not found");
}

createRoot(container).render(
  <PopcornProvider>
    <StrictMode>
      <BrowserRouter basename={import.meta.env.BASE_URL}>
        <AppRoutes />
      </BrowserRouter>
    </StrictMode>
  </PopcornProvider>
);
