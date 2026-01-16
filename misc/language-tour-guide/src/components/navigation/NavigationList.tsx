import { NavLink, useMatch } from "react-router";

import ChevronDown from "../../assets/chevron-down.svg?react";
import ChevronRight from "../../assets/chevron-right.svg?react";
import Circle from "../../assets/circle.svg?react";

import { useCallback, useState } from "react";
import type { NavigationTreeItem } from "../../utils/content/types";

type NavigationListProps = {
  items: NavigationTreeItem[];
  onClick?: () => void;
};
export function NavigationList({ items, onClick }: NavigationListProps) {
  return (
    <>
      {items.map((item) => {
        const hasChildren = item.children.length > 0;
        if (hasChildren) {
          return <CollapsibleItem key={item.path} item={item} onClick={onClick} />;
        }
        return <TopLevelItem key={item.path} item={item} onClick={onClick} />;
      })}
    </>
  );
}

type NavigationItemProps = {
  item: NavigationTreeItem;
  onClick?: () => void;
};

const baseItemStyle =
  "text-brown-80 flex cursor-pointer items-center rounded-md px-3 py-0.5 text-sm hover:bg-orange-100/10 ";

function CollapsibleItem({ item, onClick }: NavigationItemProps) {
  const onChildPage = useMatch({ path: item.path, end: false }) !== null;
  const [isOpen, toggleOpen] = useToggle(onChildPage);

  return (
    <li className="my-1">
      <div
        className={`mt-2 font-semibold ${baseItemStyle}`}
        onClick={toggleOpen}
      >
        <NavigationIcon type={isOpen ? "group-opened" : "group-collapsed"} />
        <span className="capitalize">{item.title}</span>
      </div>

      <ul
        className={`pl-5 transition-all duration-300 ${isOpen ? "block opacity-100" : "hidden opacity-0"}`}
      >
        {item.children.map((childItem) => (
          <NestedItem key={childItem.path} item={childItem} onClick={onClick} />
        ))}
      </ul>
    </li>
  );
}

function TopLevelItem({ item, onClick }: NavigationItemProps) {
  const onPage = useMatch(item.path) !== null;

  return (
    <li className="my-1">
      <div
        className={`mt-2 font-semibold ${baseItemStyle} ${onPage && "bg-orange-100/20 hover:bg-orange-100/20"}`}
      >
        <NavigationIcon type="item" />
        <NavLink
          to={item.path}
          className={`navigation-link pl-0 ${onPage && "font-semibold"}`}
          onClick={onClick}
        >
          {item.title}
        </NavLink>
      </div>
    </li>
  );
}

function NestedItem({ item, onClick }: NavigationItemProps) {
  const onPage = useMatch(item.path) !== null;

  return (
    <li className="my-1">
      <div
        className={`${baseItemStyle} mt-0 ${onPage && "bg-orange-100/20 hover:bg-orange-100/20"}`}
      >
        <NavigationIcon type="item" />
        <NavLink
          to={item.path}
          className={`navigation-link pl-0 ${onPage && "font-semibold"}`}
          onClick={onClick}
        >
          {item.title}
        </NavLink>
      </div>
    </li>
  );
}

type NavigationIconProps = {
  type: "group-opened" | "group-collapsed" | "item";
};
function NavigationIcon({ type }: NavigationIconProps) {
  return (
    <span className="mr-2 flex w-5 flex-shrink-0 justify-center text-xs text-orange-100">
      {type === "group-opened" && <ChevronDown className="w-5" />}
      {type === "group-collapsed" && <ChevronRight className="w-5" />}
      {type === "item" && <Circle className="w-1.5" />}
    </span>
  );
}

function useToggle(initial: boolean): [boolean, () => void] {
  const [state, setState] = useState(initial);

  const toggle = useCallback(() => {
    setState((prev) => !prev);
  }, [setState]);

  return [state, toggle];
}
