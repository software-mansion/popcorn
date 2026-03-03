import {
  useEditorResult,
  useEditorExecuting,
  useEditorQueued
} from "../../store/editors";
import StdoutResults from "../execution/StdoutResults";
import { CompilerError } from "../execution/CompilerError";
import { WarningOutput } from "../execution/WarningOutput";
import ArrowRight from "../../assets/arrow-right.svg?react";
import { useDelayedPending } from "../../utils/hooks/useDelayedPending";

type EditorResultsProps = {
  id: string;
};

export function Results({ id }: EditorResultsProps) {
  const result = useEditorResult(id);
  const isExecuting = useEditorExecuting(id);
  const longExecuting = useDelayedPending(isExecuting);
  const isQueued = useEditorQueued(id);

  if (!result) {
    return null;
  }

  const { stdoutResult, stderrResult, errorMessage, durationMs, output } =
    result;

  //NOTE - if there is short execution time we are showing previous outputs when executing takes more than 300ms outputs are hidden and pending state is shown
  const hasNoOutput =
    !errorMessage &&
    (!stdoutResult || stdoutResult.length === 0) &&
    (!stderrResult || stderrResult.length === 0) &&
    !output;

  if (hasNoOutput) {
    return null;
  }

  return (
    <div
      className={`text-brown-90 bg-light-30 border-grey-20 flex flex-col gap-2 rounded-b-md border-t px-4 py-3 ${isQueued ? "opacity-60" : ""}`}
    >
      <div className="text-brown-90/60 flex items-center gap-2 text-sm">
        <span className="">Output</span>
        <span className="text-xs font-light">
          {!longExecuting &&
            durationMs !== undefined &&
            `${durationMs.toFixed(3)}ms`}
        </span>
      </div>

      {!longExecuting && (
        <>
          {errorMessage && <CompilerError message={errorMessage} />}

          {stderrResult && stderrResult.length > 0 && (
            <WarningOutput stderr={stderrResult} />
          )}

          {stdoutResult && stdoutResult.length > 0 && (
            <StdoutResults stdout={stdoutResult} />
          )}

          {output && (
            <div className="flex items-center gap-3 text-sm">
              <ArrowRight className="w-3.5" />
              <span className="font-mono text-orange-100">{output}</span>
            </div>
          )}
        </>
      )}
    </div>
  );
}
