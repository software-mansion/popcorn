import XCircleIcon from "../../assets/x-circle.svg?react";
import WarningIcon from "../../assets/warning.svg?react";
import type { PropsWithChildren } from "react";

type MessageType = "error" | "warning";

type MessageOutputProps = {
  type: MessageType;
};

const styles = {
  error: {
    container: "border-red-200 bg-red-50",
    label: "text-red-700"
  },
  warning: {
    container: "border-yellow-200 bg-yellow-50",
    label: "text-yellow-700"
  }
};

export function MessageOutput({
  type,
  children
}: PropsWithChildren<MessageOutputProps>) {
  const Icon = type === "error" ? XCircleIcon : WarningIcon;
  const style = styles[type];
  const label = type === "error" ? "Error" : "Warning";

  return (
    <div
      className={`my-2 flex flex-col gap-2 rounded-md border p-3 ${style.container}`}
    >
      <div className={`flex items-center gap-2 ${style.label}`}>
        <Icon className="h-4 w-4" />
        <p className="text-sm font-medium">{label}</p>
      </div>
      {children}
    </div>
  );
}
