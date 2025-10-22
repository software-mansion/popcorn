type StdoutResultsProps = {
  stdout: string[];
};

export default function StdoutResults({ stdout }: StdoutResultsProps) {
  return (
    <div className="mt-2 rounded border-l-2 border-blue-400 bg-gray-50 p-2 text-sm whitespace-pre-wrap">
      <div className="mb-1 flex flex-col gap-1.5 text-sm font-medium">
        <span className="text-grey-70">Output:</span>
        {stdout.map((line, index) => (
          <span
            key={`stdout-${index}-${line}`}
            className="text-brown-90/70 font-mono break-words"
          >
            {line}
          </span>
        ))}
      </div>
    </div>
  );
}
