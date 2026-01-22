import { createPortal } from "react-dom";
import type { ReactNode } from "react";
import CloseIcon from "../assets/close.svg?react";

type ToastProps = {
  visible: boolean;
  onClose?: () => void;
  icon?: ReactNode;
  bgColor?: string;
  borderColor?: string;
  textColor?: string;
  children: ReactNode;
};

export function Toast({
  visible,
  onClose,
  icon,
  bgColor = "bg-orange-20",
  borderColor = "border-orange-100",
  textColor = "text-orange-200",
  children
}: ToastProps) {
  const container = document.getElementById("toast-root");

  if (!container || !visible) return null;

  return createPortal(
    <div
      role="alert"
      className={`fixed top-32 right-6 z-20 flex w-max items-center gap-3 rounded-lg border px-4 py-3 shadow-lg transition-all duration-300 ${bgColor} ${borderColor} ${textColor}`}
    >
      {icon}
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
