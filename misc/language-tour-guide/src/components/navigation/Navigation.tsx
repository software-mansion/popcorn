import { useState } from "react";
import { useLocation } from "react-router";

import { NavigationItem } from "./NavigationItem";
import Hamburger from "../../assets/hamburger.svg?react";
import Close from "../../assets/close.svg?react";
import { useNavigation } from "../../utils/hooks/useNavigation";

export function Navigation() {
  const location = useLocation();
  const [isOpen, setIsOpen] = useState(false);
  const { navigation, isLoading } = useNavigation();

  const openMenu = () => {
    setIsOpen(true);
  };

  const closeMenu = () => {
    setIsOpen(false);
  };

  if (isLoading || !navigation) {
    return (
      <span className="flex h-full items-center justify-center py-4">
        Loading...
      </span>
    );
  }

  return (
    <nav className="border-grey-20 bg-light-20 flex h-full items-center gap-6 border-b px-4 py-4 lg:px-8">
      <button className="cursor-pointer" onClick={openMenu}>
        <span className="sr-only">Open menu</span>
        <Hamburger />
      </button>
      <span className="">{location.pathname}</span>

      <ul
        className={`${isOpen ? "block" : "hidden"} scrollbar bg-light-20 absolute top-0 left-0 z-30 m-0 h-full list-none overflow-y-auto px-4 py-4 pr-12`}
      >
        <button className="cursor-pointer" onClick={closeMenu}>
          <span className="sr-only">Close menu</span>
          <Close className="w-5" />
        </button>
        {Object.entries(navigation).map(([key, item]) => {
          return <NavigationItem key={key} item={item} onClick={closeMenu} />;
        })}
      </ul>
      <div
        className={`${isOpen ? "block" : "hidden"} absolute inset-0 z-20 h-full w-full bg-black/20`}
        aria-hidden={!isOpen}
        onClick={closeMenu}
      ></div>
    </nav>
  );
}
