interface ButtonProps {
  title: string;
  type: "primary" | "secondary";
  onClick?: () => void;
}

const primaryClasses = "bg-orange-100 text-white hover:bg-orange-100/90";
const secondaryClasses = "bg-grey-20 text-brown-90 hover:bg-grey-20/80";

export function Button({ title, type, onClick }: ButtonProps) {
  return (
    <button
      className={`font-inter cursor-pointer rounded px-4 py-2 text-sm transition-colors duration-150 ${
        type === "primary" ? primaryClasses : secondaryClasses
      }`}
      onClick={onClick}
    >
      {title}
    </button>
  );
}
