import { MessageOutput } from "./MessageOutput";

type StderrOutputProps = {
  stderr: string[];
};

export function WarningOutput({ stderr }: StderrOutputProps) {
  return (
    <MessageOutput type="warning">
      <div className="flex flex-col gap-1">
        {stderr.map((line, index) => (
          <span
            key={`stderr-${index}-${line}`}
            className="whi font-mono text-xs font-medium break-words whitespace-pre-wrap text-yellow-700"
          >
            {line}
          </span>
        ))}
      </div>
    </MessageOutput>
  );
}
