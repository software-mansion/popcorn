import Logo from "../../assets/logo.svg?react";

export function PopcornLoader() {
  return (
    <div className="bg-light-30/90 flex h-full w-full grow items-center justify-center">
      <div className="flex flex-col items-center gap-4">
        <Logo className="h-16 w-16 animate-bounce" />
        <p className="font-mono text-sm font-medium">Initializing Popcorn...</p>
      </div>
    </div>
  );
}
