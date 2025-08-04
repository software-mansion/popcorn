import { useEffect } from "react";
import Button from "./Button";

function Results() {
  const handleRunCode = () => {
    console.log("Run Code!");
  };

  const onKeyDown = (e: KeyboardEvent) => {
    if ((e.metaKey || e.ctrlKey) && e.key === "Enter") {
      handleRunCode();
    }
  };

  useEffect(() => {
    document.addEventListener("keydown", onKeyDown);

    return () => {
      document.removeEventListener("keydown", onKeyDown);
    };
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, []);

  return (
    <section className="min-h-60 bg-light-30 rounded-md border pb-6 border-grey-20">
      <div className="flex flex-wrap gap-3 py-3 justify-end pr-6 border-b border-orange-100 w-full">
        <Button title="Reset Code" type="secondary" />
        <Button title="Format Code" type="secondary" />
        <Button title="Run Code" type="primary" onClick={handleRunCode} />
      </div>
      <div className="font-inter text-brown-90 mt-4 px-6">Results</div>
    </section>
  );
}

export default Results;
