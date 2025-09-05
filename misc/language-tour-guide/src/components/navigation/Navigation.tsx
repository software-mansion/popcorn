import { useState } from "react";
import { useLocation } from "react-router";

import { NavigationItem } from "./NavigationItem";
import { useNavigation } from "../../utils/hooks/useNavigation";

import Hamburger from "../../assets/hamburger.svg?react";
import Close from "../../assets/close.svg?react";

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
    <nav className="border-grey-20 bg-light-20 flex h-12 items-center gap-8 border-b px-4 py-3 lg:px-8">
      <button
        className="orange-shadow cursor-pointer rounded-md bg-orange-100 p-0.5"
        onClick={openMenu}
      >
        <span className="sr-only">Open menu</span>
        <Hamburger className="h-full w-5.5 text-white" />
      </button>
      <span className="ml-5">{location.pathname}</span>
      <ul
        className={`scrollbar bg-light-20 absolute top-16 left-0 z-30 m-0 h-[calc(100%-theme(space.16))] list-none overflow-y-auto px-4 py-3 pr-12 transition-all duration-300 ease-in-out ${
          isOpen
            ? "translate-x-0 opacity-100"
            : "pointer-events-none -translate-x-full opacity-0"
        }`}
      >
        <button
          className="orange-shadow mb-3 cursor-pointer rounded-md bg-orange-100 p-0.5 lg:ml-4"
          onClick={closeMenu}
        >
          <span className="sr-only">Close menu</span>
          <Close className="h-full w-5.5 text-white" />
        </button>
        {navigation.map((item) => (
          <NavigationItem key={item.path} item={item} onClick={closeMenu} />
        ))}
      </ul>
      <div
        className={`absolute inset-0 z-20 h-full w-full bg-black/20 transition-opacity duration-300 ease-in-out ${
          isOpen ? "opacity-100" : "pointer-events-none opacity-0"
        }`}
        aria-hidden={!isOpen}
        onClick={closeMenu}
      ></div>
    </nav>
  );
}
