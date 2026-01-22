import { Toast } from "../Toast";
import CheckCircle from "../../assets/check-circle.svg?react";

type EditorToastProps = {
  isVisible: boolean;
  onClick?: () => void;
  onClose?: () => void;
};

export function EditorToast({ isVisible, onClick, onClose }: EditorToastProps) {
  if (!isVisible) return null;

  return (
    <Toast
      visible
      onClose={onClose}
      icon={<CheckCircle className="h-4 w-4" />}
      bgColor="bg-light-30"
      borderColor="border-grey-20"
      textColor="text-brown-70"
    >
      <span className="font-inter text-sm font-medium">Code Completed</span>
      <button
        className={`text-brown-60 cursor-pointer text-xs`}
        onClick={onClick}
      >
        Click to scroll
      </button>
    </Toast>
  );
}
