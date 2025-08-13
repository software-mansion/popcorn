import { NavLink, useLocation } from "react-router";
import type { NavigationTreeItem } from "../../utils/content/navigation";

import ChevronDown from "../../assets/chevron-down.svg?react";
import ChevronRight from "../../assets/chevron-right.svg?react";
import { useState } from "react";

type NavigationItemProps = {
  item: NavigationTreeItem;
  onClick?: () => void;
};

export function NavigationItem({ item, onClick }: NavigationItemProps) {
  const { hash, pathname } = useLocation();

  const [isOpen, setIsOpen] = useState(
    `${pathname}${hash}`.includes(item.path ?? "")
  );

  const hasChildren = item.children !== undefined;

  const isSelected = (item: NavigationTreeItem) => {
    return item.path && item.path.includes(hash);
  };

  return (
    <li className="my-1">
      <div
        className={`flex items-center rounded-md px-3 py-0.5 text-sm ${
          hasChildren
            ? "text-brown-header hover:bg-orange-20 mt-2 cursor-pointer font-semibold"
            : "text-brown-80"
        }`}
        onClick={() => hasChildren && setIsOpen((prev) => !prev)}
      >
        {hasChildren && (
          <span className="mr-2 text-xs text-orange-100">
            {isOpen ? (
              <ChevronDown className="w-5" />
            ) : (
              <ChevronRight className="w-5" />
            )}
          </span>
        )}
        {item.path ? (
          <NavLink
            to={item.path}
            className={({ isActive }) =>
              `flex w-full items-center rounded-md px-3 py-0.5 text-sm transition-colors duration-200 ${
                isActive && isSelected(item)
                  ? "bg-color-orange-20 text-color-orange-100 font-semibold"
                  : "hover:bg-orange-20 text-color-brown-80 hover:text-color-brown-100"
              } ${hasChildren ? "pl-0" : "pl-5"}`
            }
            onClick={onClick}
          >
            {item.title}
          </NavLink>
        ) : (
          <span className="capitalize">{item.title}</span>
        )}
      </div>

      {hasChildren && (
        <ul
          className={`pl-5 transition-all duration-300 ${isOpen ? "block opacity-100" : "hidden opacity-0"}`}
        >
          {Object.entries(item.children ?? {}).map(([key, childItem]) => (
            <NavigationItem key={`${key}`} item={childItem} onClick={onClick} />
          ))}
        </ul>
      )}
    </li>
  );
}
