import { NavLink, useLocation } from "react-router";

import ChevronDown from "../../assets/chevron-down.svg?react";
import ChevronRight from "../../assets/chevron-right.svg?react";
import { useEffect, useState } from "react";
import type { NavigationTreeItem } from "../../utils/content/types";

type NavigationItemProps = {
  item: NavigationTreeItem;
  onClick?: () => void;
};

export function NavigationItem({ item, onClick }: NavigationItemProps) {
  const { hash, pathname } = useLocation();
  const [isOpen, setIsOpen] = useState(
    `${pathname}${hash}`.includes(item.path)
  );

  useEffect(() => {
    setIsOpen(`${pathname}${hash}`.includes(item.path));
  }, [pathname, hash, item.path]);

  const hasChildren = item.children.length > 0;

  const isSelected = (item: NavigationTreeItem) => {
    return item.path && item.path.includes(hash);
  };

  return (
    <li className="my-1">
      <div
        className={`flex items-center rounded-md px-3 py-0.5 text-sm ${
          hasChildren
            ? "text-brown-header mt-2 cursor-pointer font-semibold hover:bg-orange-100/10"
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
        {item.type === "link" ? (
          <NavLink
            to={item.path}
            className={({ isActive }) =>
              `flex w-full items-center rounded-md px-3 py-0.5 text-sm transition-colors duration-300 ${
                isActive && isSelected(item)
                  ? "bg-color-orange-20 text-color-orange-100 bg-orange-100/10 font-semibold"
                  : "text-color-brown-80 hover:bg-orange-100/10"
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
          {item.children.map((childItem) => (
            <NavigationItem
              key={childItem.path}
              item={childItem}
              onClick={onClick}
            />
          ))}
        </ul>
      )}
    </li>
  );
}
