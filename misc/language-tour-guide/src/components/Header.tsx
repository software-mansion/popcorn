import { Link } from "react-router";
import LogoText from "../assets/logo-text.svg?react";
import Logo from "../assets/logo.svg?react";

export function Header() {
  return (
    <nav className="text-brown-80 bg-light-20 fixed top-0 z-30 flex h-16 w-full items-center gap-6 border-b border-orange-100 px-4 py-4 lg:px-8">
      <Link to="https://popcorn.swmansion.com/" target="_blank">
        <Logo className="h-9 w-max md:hidden" />
        <LogoText className="hidden h-9 w-max md:block" />
        <p className="sr-only">popcorn</p>
      </Link>
      <Link to="/" className="font-handjet text-2xl lg:text-3xl">
        Elixir Language Tour
      </Link>
    </nav>
  );
}
