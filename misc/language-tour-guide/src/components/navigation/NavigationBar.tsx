import { NavigationButton } from "./NavigationButton";
import { useSiblingsNode } from "../../utils/hooks/useSiblingsNode";

export default function NavigationBar() {
  const { siblingsNode, childrenCount, currentIndex } = useSiblingsNode();

  return (
    <div className="bg-light-20 fixed bottom-0 left-0 z-10 flex w-full items-center justify-center gap-7 border-t border-t-orange-100 p-2 lg:sticky lg:p-3">
      <NavigationButton
        direction="previous"
        path={siblingsNode?.previousNode?.path}
      />
      {currentIndex}/{childrenCount}
      <NavigationButton direction="next" path={siblingsNode?.nextNode?.path} />
    </div>
  );
}
