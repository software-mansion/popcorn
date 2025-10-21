import { MessageOutput } from "./MessageOutput";

type CompilerErrorProps = {
  message: string;
};

export function CompilerError({ message }: CompilerErrorProps) {
  return (
    <MessageOutput type="error">
      <pre className="font-mono text-xs break-words whitespace-pre-wrap text-red-600">
        {message}
      </pre>
    </MessageOutput>
  );
}
