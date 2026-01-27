import { useEditorsStore } from "../../store/editors";
import { Results } from "../execution/Results";
import CodeDisplay from "./CodeDisplay";

type CodeCellProps = {
  id: string;
};

export function CodeCell({ id }: CodeCellProps) {
  const getEditor = useEditorsStore((state) => state.getEditor);

  const editor = getEditor(id);

  if (!editor) {
    return (
      <div className="my-6 rounded border border-red-300 bg-red-50 p-4">
        <h3 className="mb-2 text-lg font-medium text-red-800">
          Editor Not Found
        </h3>
        <p className="text-red-700">
          The requested editor with ID "{id}" was not found.
        </p>
      </div>
    );
  }

  return (
    <div
      className="code-cell border-grey-20 my-2 mb-8 overflow-hidden rounded-md border lg:min-w-2xl"
      data-correct-code={editor.correctCode}
    >
      <CodeDisplay id={id} />
      <Results id={id} />
    </div>
  );
}
