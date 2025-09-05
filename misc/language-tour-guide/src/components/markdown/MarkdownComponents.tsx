import { useCallback, type ComponentProps } from "react";
import { useCodeEditorStore } from "../../store/codeEditor";

export const Heading1 = ({ children, ...props }: ComponentProps<"h1">) => (
  <h1 className="mb-6 text-3xl font-bold text-gray-900" {...props}>
    {children}
  </h1>
);

export const Heading2 = ({ children, ...props }: ComponentProps<"h2">) => (
  <h2 className="mt-8 mb-4 text-2xl font-semibold text-gray-800" {...props}>
    {children}
  </h2>
);

export const Heading3 = ({ children, ...props }: ComponentProps<"h3">) => (
  <h3 className="mt-6 mb-3 text-xl font-medium text-gray-800" {...props}>
    {children}
  </h3>
);

export const Paragraph = ({ children, ...props }: ComponentProps<"p">) => (
  <p className="mb-4 leading-relaxed text-gray-700" {...props}>
    {children}
  </p>
);

export const UnorderedList = ({ children, ...props }: ComponentProps<"ul">) => (
  <ul className="mb-4 list-disc pl-6" {...props}>
    {children}
  </ul>
);

export const OrderedList = ({ children, ...props }: ComponentProps<"ol">) => (
  <ol className="mb-4 list-decimal pl-6" {...props}>
    {children}
  </ol>
);

export const ListItem = ({ children, ...props }: ComponentProps<"li">) => (
  <li className="mb-2" {...props}>
    {children}
  </li>
);

export const InlineCode = ({ children, ...props }: ComponentProps<"code">) => (
  <code
    className="bg-orange-20 rounded px-1 py-0.5 font-mono text-sm text-pink-600"
    {...props}
  >
    {children}
  </code>
);

export const BlockQuote = ({
  children,
  ...props
}: ComponentProps<"blockquote">) => (
  <blockquote
    className="my-4 border-l-4 border-gray-200 pl-4 text-gray-600 italic"
    {...props}
  >
    {children}
  </blockquote>
);

type CodeBlockProps = ComponentProps<"pre"> & {
  rawCode?: string;
};

export const CodeBlock = ({ children, rawCode, ...props }: CodeBlockProps) => {
  const setCode = useCodeEditorStore((state) => state.setCode);

  const handleUseInEditor = useCallback(() => {
    if (rawCode) {
      setCode(rawCode);
    }
  }, [rawCode, setCode]);

  return (
    <pre
      className="text-brown-90 bg-grey-10 relative my-4 overflow-x-auto rounded p-4 font-mono text-sm"
      {...props}
    >
      {children}

      {rawCode && (
        <button
          onClick={handleUseInEditor}
          className="text-brown-90 absolute right-0 bottom-0 m-2 flex cursor-pointer items-center rounded bg-orange-100/80 px-2 py-1 text-xs transition-colors duration-200 hover:bg-orange-100"
          aria-label="Use in editor"
        >
          <svg
            xmlns="http://www.w3.org/2000/svg"
            className="mr-1 h-3.5 w-3.5"
            viewBox="0 0 20 20"
            fill="currentColor"
          >
            <path d="M13.586 3.586a2 2 0 112.828 2.828l-.793.793-2.828-2.828.793-.793zM11.379 5.793L3 14.172V17h2.828l8.38-8.379-2.83-2.828z" />
          </svg>
          Try
        </button>
      )}
    </pre>
  );
};

export const Link = ({ children, ...props }: ComponentProps<"a">) => (
  <a className="text-blue-600 underline hover:text-blue-800" {...props}>
    {children}
  </a>
);
