import { createPortal } from "react-dom";
import type { PropsWithChildren } from "react";
import CloseIcon from "../assets/close.svg?react";

type ToastProps = {
  visible: boolean;
  onClose?: () => void;
  bgColor?: string;
  borderColor?: string;
  textColor?: string;
};

export function Toast({
  visible,
  onClose,
  bgColor = "bg-orange-20",
  borderColor = "border-orange-100",
  textColor = "text-orange-200",
  children
}: PropsWithChildren<ToastProps>) {
  const container = document.getElementById("toast-root");

  if (!container || !visible) return null;

  return createPortal(
    <div
      role="alert"
      className={`top-32 right-6 z-20 flex w-max items-center gap-4 rounded-lg border px-4 py-3 shadow-lg transition-all duration-300 ${bgColor} ${borderColor} ${textColor}`}
    >
      {children}
      {onClose && (
        <button
          type="button"
          onClick={(e) => {
            e.stopPropagation();
            onClose();
          }}
          className="ml-1 cursor-pointer rounded p-0.5 opacity-70 transition-opacity hover:opacity-100"
          aria-label="Close"
        >
          <CloseIcon className="h-4 w-4" />
        </button>
      )}
    </div>,
    container
  );
}
