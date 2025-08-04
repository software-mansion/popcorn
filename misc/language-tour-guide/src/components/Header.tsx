import { Link } from "react-router";
import Logo from "../assets/logo.svg?react";

function Header() {
  return (
    <nav className="py-4 lg:px-8 px-4 top-0 h-16 fixed w-full z-10 text-brown-80 bg-light-20 border-b border-orange-100">
      <Link
        to="/"
        className="flex gap-6 items-center lg:text-3xl text-2xl font-handjet"
      >
        <Logo className="h-9 w-max" /> Language Tour Guide
      </Link>
    </nav>
  );
}

export default Header;
