import type { ExecutionState } from "../../store/editors";
import Spinner from "../../assets/spinner.svg?react";
import EllipsisHorizontalCircle from "../../assets/ellipsis-horizontal-circle.svg?react";
import CheckCircle from "../../assets/check-circle.svg?react";
import XCircle from "../../assets/x-circle.svg?react";
import QuestionMarkCircle from "../../assets/question-mark-circle.svg?react";
import Clock from "../../assets/clock.svg?react";

type ExecutionStateBadgeProps = {
  state: ExecutionState;
};

const getStateConfig = (state: ExecutionState) => {
  switch (state) {
    case "not_run":
      return {
        label: "Not run",
        bgColor: "bg-grey-10",
        textColor: "text-brown-60",
        borderColor: "border-grey-20",
        icon: <EllipsisHorizontalCircle className="h-3.5 w-3.5" />
      };
    case "success":
      return {
        label: "Success",
        bgColor: "bg-light-30",
        textColor: "text-brown-70",
        borderColor: "border-grey-20",
        icon: <CheckCircle className="h-3.5 w-3.5" />
      };
    case "failure":
      return {
        label: "Failed",
        bgColor: "bg-red-50",
        textColor: "text-red-700",
        borderColor: "border-red-200",
        icon: <XCircle className="h-3.5 w-3.5" />
      };
    case "stale":
      return {
        label: "Stale",
        bgColor: "bg-yellow-50",
        textColor: "text-yellow-700",
        borderColor: "border-yellow-200",
        icon: <QuestionMarkCircle className="h-3.5 w-3.5" />
      };
    case "running":
      return {
        label: "Running",
        bgColor: "bg-orange-20",
        textColor: "text-orange-200",
        borderColor: "border-orange-100",
        icon: <Spinner className="h-3.5 w-3.5 animate-spin" />
      };
    case "queued":
      return {
        label: "Queued",
        bgColor: "bg-grey-10",
        textColor: "text-brown-80",
        borderColor: "border-grey-20",
        icon: <Clock className="h-3.5 w-3.5" />
      };
  }
};

export function ExecutionStateBadge({ state }: ExecutionStateBadgeProps) {
  const config = getStateConfig(state);

  return (
    <div
      data-testid="execution-state-badge"
      data-state={state}
      className={`flex items-center gap-1.5 rounded-full border px-2.5 py-1 text-xs font-medium ${config.bgColor} ${config.textColor} ${config.borderColor}`}
    >
      {config.icon}
      <span>{config.label}</span>
    </div>
  );
}
