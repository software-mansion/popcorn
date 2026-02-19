import { Button } from "../Button";

type PopcornErrorProps = {
  onRetry: () => void;
};

export function PopcornError({ onRetry }: PopcornErrorProps) {
  return (
    <div className="absolute top-1/2 left-1/2 flex w-full -translate-x-1/2 -translate-y-1/2 flex-col items-center justify-center gap-4 px-4">
      <p className="text-center text-lg">
        Failed to start the Popcorn. Please try again.
      </p>
      <Button type="primary" onClick={onRetry} title="Retry" />
    </div>
  );
}
