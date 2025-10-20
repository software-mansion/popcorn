type StderrOutputProps = {
  stderr: string[];
};

export function StderrOutput({ stderr }: StderrOutputProps) {
  return (
    <div className="flex flex-col gap-1">
      {stderr.map((line, index) => (
        <span
          key={`stderr-${index}-${line}`}
          className="text-red-700 text-xs font-medium whitespace-pre-wrap"
        >
          {line}
        </span>
      ))}
    </div>
  );
}
