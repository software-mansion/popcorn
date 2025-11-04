import { usePopcorn } from "../../context/popcorn/actions";

import Logo from "../../assets/logo.svg?react";

export function PopcornLoader() {
  const { isLoadingPopcorn } = usePopcorn();

  return (
    <>
      {isLoadingPopcorn && (
        <div className="bg-light-30/90 absolute inset-0 z-10 flex items-center justify-center rounded-md backdrop-blur-sm">
          <div className="flex flex-col items-center gap-4">
            <Logo className="h-16 w-16 animate-bounce" />
            <p className="text-dark-50 font-mono text-sm font-medium">
              Initializing Popcorn...
            </p>
          </div>
        </div>
      )}
    </>
  );
}
