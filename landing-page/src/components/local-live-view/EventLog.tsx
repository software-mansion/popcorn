import { useStore } from "@nanostores/react";
import { $log } from "../../scripts/local-live-view/store";
import "./llv-scrollbar.css";
import { PanelLayout } from "./PanelLayout";

export function EventLog() {
  const log = useStore($log);
  if (log.length === 0) return null;

  return (
    <PanelLayout title="event log">
      <div className="llv-scroll flex max-h-32 flex-col gap-1 overflow-hidden overflow-y-auto text-white">
        {log.map((entry) => (
          <div
            key={entry.id}
            className="animate-fade-slide flex gap-2 font-mono text-xs"
          >
            <span className="text-orange-100">{entry.event}</span>
            <span className="text-llv-green-60">{entry.result}</span>
          </div>
        ))}
      </div>
    </PanelLayout>
  );
}
