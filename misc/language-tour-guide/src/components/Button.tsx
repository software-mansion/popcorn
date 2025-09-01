interface ButtonProps {
  title: string;
  type: "primary" | "secondary";
  disabled?: boolean;
  onClick?: () => void;
}

const primaryClasses =
  "bg-orange-100 text-white hover:bg-orange-100/90 orange-shadow disabled:shadow-none";
const secondaryClasses = "bg-grey-20 text-brown-90 hover:bg-grey-20/80";

export function Button({ title, type, disabled, onClick }: ButtonProps) {
  return (
    <button
      disabled={disabled}
      className={`disabled:bg-brown-gray font-inter cursor-pointer rounded px-4 py-2 text-sm transition-colors duration-300 disabled:cursor-not-allowed ${
        type === "primary" ? primaryClasses : secondaryClasses
      }`}
      onClick={onClick}
    >
      {title}
    </button>
  );
}
