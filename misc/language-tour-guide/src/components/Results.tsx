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
    <section className="bg-light-30 border-grey-20 min-h-60 rounded-md border pb-6">
      <div className="flex w-full flex-wrap justify-end gap-3 border-b border-orange-100 py-3 pr-6">
        <Button title="Reset Code" type="secondary" />
        <Button title="Format Code" type="secondary" />
        <Button title="Run Code" type="primary" onClick={handleRunCode} />
      </div>
      <div className="font-inter text-brown-90 mt-4 px-6">Results</div>
    </section>
  );
}

export default Results;
