import { NavLink } from "react-router";
import ChevronRight from "../../assets/chevron-right.svg?react";
import ChevronLeft from "../../assets/chevron-left.svg?react";

type Direction = "next" | "previous";
type NavigationButtonProps = {
  direction: Direction;
  path?: string;
  isChapterNavigation?: boolean;
};

export function NavigationButton({
  direction,
  path,
  isChapterNavigation
}: NavigationButtonProps) {
  const Icon = direction === "next" ? ChevronRight : ChevronLeft;
  const label = direction === "next" ? "Next section" : "Previous section";
  const chapterLabel =
    direction === "next" ? "Next chapter" : "Previous chapter";

  const className = `w-44 flex ${direction === "previous" ? "justify-end" : ""}`;

  if (!path) {
    return (
      <button
        disabled
        className={`group cursor-not-allowed ${className}`}
        aria-hidden="true"
      >
        <Icon className="border-brown-60 group-disabled:bg-grey-20 group-disabled:text-brown-60 w-8 rounded-full border p-1 shadow-md shadow-black/10 lg:w-9" />
      </button>
    );
  }

  if (isChapterNavigation) {
    return (
      <div className={`h-8 w-44 lg:h-9 ${className}`}>
        <NavLink
          to={path}
          className={`flex w-fit items-center gap-1 rounded-full bg-orange-100 py-1 text-sm font-medium text-white transition-colors duration-300 hover:bg-orange-200 lg:py-0`}
        >
          {direction === "previous" && <Icon className="w-8 p-1 lg:w-9" />}
          <span
            className={`${direction === "next" ? "pl-4" : "pr-4"} @max-md/main:text-[0.625rem]`}
          >
            {chapterLabel}
          </span>
          {direction === "next" && <Icon className="w-8 p-1 lg:w-9" />}
        </NavLink>
      </div>
    );
  }

  return (
    <div className={className}>
      <NavLink to={path} className="w-min">
        <span className="sr-only">{label}</span>
        <Icon className="orange-shadow w-8 rounded-full bg-orange-100 p-1 text-white transition-all duration-300 hover:bg-orange-200 lg:w-9" />
      </NavLink>
    </div>
  );
}
