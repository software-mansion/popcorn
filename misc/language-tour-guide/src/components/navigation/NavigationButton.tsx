import { NavLink } from "react-router";
import ChevronRight from "../../assets/chevron-right.svg?react";
import ChevronLeft from "../../assets/chevron-left.svg?react";

type Direction = "next" | "previous";
type NavigationButtonProps = {
  direction: Direction;
  path?: string;
};

export function NavigationButton({ direction, path }: NavigationButtonProps) {
  const Icon = direction === "next" ? ChevronRight : ChevronLeft;
  const label = direction === "next" ? "Next section" : "Previous section";

  return path ? (
    <NavLink to={path}>
      <span className="sr-only">{label}</span>
      <Icon className="nav-arrow w-5.5" />
    </NavLink>
  ) : (
    <button disabled className="group cursor-not-allowed" aria-hidden="true">
      <Icon className="nav-arrow-disabled w-5.5 group-disabled:opacity-50" />
    </button>
  );
}
