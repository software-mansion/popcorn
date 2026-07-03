import { useStore } from "@nanostores/react";
import { $assigns, $flashKeys } from "../../scripts/local-live-view/store";
import { PanelLayout } from "./PanelLayout";

export function AssignsPanel() {
  const assigns = useStore($assigns);
  const flashKeys = useStore($flashKeys);
  const entries = Object.entries(assigns);

  return (
    <PanelLayout title="socket.assigns">
      <div className="font-mono text-sm leading-loose">
        <span className="text-llv-brown-55">%&#123;</span>
        {entries.map(([k, v], i) => (
          <div key={k} className="block">
            &nbsp;&nbsp;
            <span className="text-llv-orange-50">{k}:</span>{" "}
            <span
              className={`transition-colors duration-[250ms] ${flashKeys.has(k) ? "text-orange-100" : "text-light-20"}`}
            >
              {typeof v === "string" ? `"${v}"` : String(v)}
            </span>
            {i < entries.length - 1 && (
              <span className="text-llv-brown-55">,</span>
            )}
          </div>
        ))}
        <span className="text-llv-brown-55">&#125;</span>
      </div>
    </PanelLayout>
  );
}
