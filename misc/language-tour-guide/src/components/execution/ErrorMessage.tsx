import XCircleIcon from "../../assets/x-circle.svg?react";

type ErrorMessageProps = {
  message: string;
};

export function ErrorMessage({ message }: ErrorMessageProps) {
  return (
    <div className="my-2 flex flex-col gap-2 rounded-md border border-red-200 bg-red-50 p-3">
      <div className="flex items-center gap-2">
        <XCircleIcon className="h-4 w-4 text-red-700" />
        <p className="text-sm font-medium text-red-700">Error</p>
      </div>
      <pre className="mt-1 text-xs break-words whitespace-pre-wrap text-red-600">
        {message}
      </pre>
    </div>
  );
}
