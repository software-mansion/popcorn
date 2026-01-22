import { Link } from "react-router";
import LogoText from "../assets/logo-text.svg?react";
import Logo from "../assets/logo.svg?react";

export function Branding() {
  return (
    <nav className="text-brown-80 bg-light-20 top-0 z-30 flex h-16 w-full items-center gap-2 border-b border-orange-100 px-4 py-4 sm:gap-6 lg:px-8">
      <Link to="https://popcorn.swmansion.com/" target="_blank">
        <Logo className="h-9 w-max md:hidden" />
        <LogoText className="hidden h-9 w-max md:block" />
        <p className="sr-only">popcorn</p>
      </Link>
      <Link to="/" className="font-handjet text-xl sm:text-3xl">
        Elixir Language Tour
      </Link>
      <div className="text-brown-70 text-2xs ml-auto flex flex-col text-right sm:text-sm">
        <span>Created by Software Mansion</span>
        <Link
          to="https://swmansion.com/contact"
          target="_blank"
          className="font-medium text-orange-200 underline transition-colors hover:text-orange-100"
        >
          Hire us
        </Link>
      </div>
    </nav>
  );
}
