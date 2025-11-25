import Logo from "../assets/logo.svg?react";

type LoaderProps = {
  message?: string;
};

export function Loader({ message = "Loading content..." }: LoaderProps) {
  return (
    <div className="absolute top-1/2 left-1/2 flex -translate-x-1/2 -translate-y-1/2 transform flex-col items-center gap-4">
      <Logo className="h-16 w-16 animate-bounce" />
      <p className="`font-mono text-lg font-medium">{message}</p>
    </div>
  );
}
