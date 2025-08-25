import { Link } from "react-router";
import LogoText from "../assets/logo-text.svg?react";
import Logo from "../assets/logo.svg?react";

export function Header() {
  return (
    <nav className="text-brown-80 bg-light-20 fixed top-0 z-10 h-16 w-full border-b border-orange-100 px-4 py-4 lg:px-8">
      <Link
        to="/"
        className="font-handjet flex items-center gap-6 text-2xl lg:text-3xl"
      >
        <Logo className="h-9 w-max md:hidden" />
        <LogoText className="hidden h-9 w-max md:block" /> Language Tour Guide
      </Link>
    </nav>
  );
}
