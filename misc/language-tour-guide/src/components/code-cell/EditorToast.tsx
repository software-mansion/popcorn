import { Toast } from "../Toast";
import ArrowUturnUP from "../../assets/arrow-uturn-up.svg?react";

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
      bgColor="bg-light-30"
      borderColor="border-grey-20"
      textColor="text-brown-70"
    >
      <button
        className="flex cursor-pointer items-center gap-2"
        onClick={onClick}
      >
        <ArrowUturnUP className="h-4 w-4" />
        <span className="font-inter text-sm font-medium">Code Completed</span>
      </button>
    </Toast>
  );
}
