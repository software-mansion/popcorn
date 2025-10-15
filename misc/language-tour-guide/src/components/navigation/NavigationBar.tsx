import { NavigationButton } from "./NavigationButton";
import { useSiblingsNode } from "../../utils/hooks/useSiblingsNode";

export default function NavigationBar() {
  const { siblingsNode, childrenCount, currentIndex } = useSiblingsNode();

  const isAtFirstInChapter = currentIndex === 1 || childrenCount === 0;
  const isAtLastInChapter = currentIndex === childrenCount;
  const isTopLevelChapter = childrenCount !== 0 || currentIndex !== 0;
  const navigationClassName = isTopLevelChapter ? "block" : "invisible";

  return (
    <div className="bg-light-20 fixed bottom-0 left-0 z-10 flex w-full items-center justify-center gap-7 border-t border-t-orange-100 p-2 lg:sticky lg:p-3">
      <NavigationButton
        direction="previous"
        path={siblingsNode?.previousNode?.path}
        isChapterNavigation={isAtFirstInChapter}
      />
      <span className={navigationClassName} aria-hidden={!isTopLevelChapter}>
        {currentIndex}/{childrenCount}
      </span>
      <NavigationButton
        direction="next"
        path={siblingsNode?.nextNode?.path}
        isChapterNavigation={isAtLastInChapter}
      />
    </div>
  );
}
