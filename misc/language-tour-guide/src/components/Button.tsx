interface ButtonProps {
  title: string;
  type: "primary" | "secondary";
  onClick?: () => void;
}

const primaryClasses = "bg-orange-100 text-white hover:bg-orange-100/90";
const secondaryClasses = "bg-grey-20 text-brown-90 hover:bg-grey-20/80";

function Button({ title, type, onClick }: ButtonProps) {
  return (
    <button
      className={`px-4 py-2 rounded font-inter text-sm transition-colors duration-150 cursor-pointer ${
        type === "primary" ? primaryClasses : secondaryClasses
      }`}
      onClick={onClick}
    >
      {title}
    </button>
  );
}

export default Button;
