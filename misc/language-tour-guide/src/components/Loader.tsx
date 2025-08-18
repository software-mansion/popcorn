type LoaderProps = {
  message?: string;
};

export function Loader({ message = "Loading content..." }: LoaderProps) {
  return (
    <div className="flex min-h-[200px] flex-col items-center justify-center space-y-4 p-8">
      <p className="animate-pulse text-lg font-medium text-gray-600">
        {message}
      </p>
    </div>
  );
}
