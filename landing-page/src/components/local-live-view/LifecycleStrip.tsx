import { useStore } from "@nanostores/react";
import { $step } from "../../scripts/local-live-view/store";

const STEPS = [
  "phx-event",
  "handle_event",
  "update assigns",
  "re-render",
];

export function LifecycleStrip() {
  const step = useStore($step);

  return (
    <div className="text-llv-brown-50 bg-llv-bg-110 flex flex-wrap items-center gap-1 overflow-hidden rounded-xl px-4 py-3 font-mono text-xs">
      {STEPS.map((s, i) => (
        <span key={s}>
          <span
            className={`shrink-0 rounded-sm px-2 py-1 whitespace-nowrap transition-all duration-300 ${step === i ? "bg-orange-100/15 text-orange-100" : ""}`}
          >
            {s}
          </span>
          {i < STEPS.length - 1 && (
            <span
              className={`text-llv-brown-100 shrink-0 transition-all duration-300 ${step === i ? "text-orange-100" : ""}`}
            >
              →
            </span>
          )}
        </span>
      ))}
    </div>
  );
}
