type StdoutResultsProps = {
  stdout: string[];
};

export default function StdoutResults({ stdout }: StdoutResultsProps) {
  return (
    <div className="text-brown-100 flex flex-col gap-1 rounded bg-gray-50 text-sm whitespace-pre-wrap">
      {stdout.map((line, index) => (
        <span key={`stdout-${index}-${line}`} className="font-mono break-words">
          {line}
        </span>
      ))}
    </div>
  );
}
