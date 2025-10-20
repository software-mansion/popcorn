import XCircleIcon from "../../assets/x-circle.svg?react";
import { CompilerError } from "./CompilerError";
import { StderrOutput } from "./StderrOutput";

type ErrorMessageProps = {
  message: string | null;
  stderr?: string[];
};

export function ErrorMessage({ message, stderr }: ErrorMessageProps) {
  if (!message && (!stderr || stderr.length === 0)) {
    return null;
  }

  const hasStderr = stderr && stderr.length > 0;

  return (
    <div className="my-2 flex flex-col gap-2 rounded-md border border-red-200 bg-red-50 p-3">
      <div className="flex items-center gap-2">
        <XCircleIcon className="h-4 w-4 text-red-700" />
        <p className="text-sm font-medium text-red-700">Error</p>
      </div>

      {message && <CompilerError message={message} />}

      {message && hasStderr && <div className="border-t border-red-200 pt-2" />}

      {hasStderr && <StderrOutput stderr={stderr} />}
    </div>
  );
}
