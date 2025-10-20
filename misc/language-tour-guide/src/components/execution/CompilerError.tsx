type CompilerErrorProps = {
  message: string;
};

export function CompilerError({ message }: CompilerErrorProps) {
  return (
    <pre className="text-xs break-words whitespace-pre-wrap text-red-600">
      {message}
    </pre>
  );
}
