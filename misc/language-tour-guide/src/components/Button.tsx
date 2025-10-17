interface ButtonProps {
  title: string;
  type: keyof typeof BUTTON_CLASSES;
  className?: string;
  disabled?: boolean;
  onClick?: () => void;
}

const BUTTON_CLASSES = {
  primary:
    "bg-orange-100 text-white hover:bg-orange-100/90 orange-shadow disabled:shadow-none px-4 py-2",
  secondary: "bg-grey-20 text-brown-90 hover:bg-grey-20/80 px-4 py-2",
  tertiary:
    "bg-brown-70 hover:bg-brown-60 shadow-md rounded-md px-3 py-1 font-medium text-white"
};

export function Button({
  title,
  type,
  disabled,
  className,
  onClick
}: ButtonProps) {
  return (
    <button
      disabled={disabled}
      className={`disabled:bg-brown-gray font-inter cursor-pointer rounded text-sm transition-all duration-300 disabled:cursor-not-allowed ${BUTTON_CLASSES[type]} ${className}`}
      onClick={onClick}
    >
      {title}
    </button>
  );
}
