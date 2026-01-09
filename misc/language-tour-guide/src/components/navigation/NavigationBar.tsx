import { NavigationButton } from "./NavigationButton";
import { FeedbackButton } from "./FeedbackButton";
import { useLocation } from "react-router";
import { getNodeNavigationSiblings } from "../../utils/content/navigation-builder";

export default function NavigationBar() {
  const { pathname } = useLocation();

  const { siblingsNode, siblingCount, currentIndex } =
    getNodeNavigationSiblings(pathname);

  const isAtFirstInChapter = currentIndex === 1 || siblingCount === 0;
  const isAtLastInChapter = currentIndex === siblingCount;
  const isTopLevelChapter = siblingCount !== 0 || currentIndex !== 0;

  const navigationClassName = isTopLevelChapter ? "block" : "invisible";

  return (
    <div className="sticky bottom-0 left-0 z-10 flex w-full flex-col items-center gap-3">
      <FeedbackButton className="lg:hidden" />
      <div className="bg-light-20 @container/main flex w-full items-center justify-center gap-7 border-t border-t-orange-100 p-2 lg:p-3">
        <NavigationButton
          direction="previous"
          path={siblingsNode?.previousNode?.path}
          isChapterNavigation={isAtFirstInChapter}
        />
        <span className={navigationClassName} aria-hidden={!isTopLevelChapter}>
          {currentIndex}/{siblingCount}
        </span>
        <NavigationButton
          direction="next"
          path={siblingsNode?.nextNode?.path}
          isChapterNavigation={isAtLastInChapter}
        />
      </div>
    </div>
  );
}
