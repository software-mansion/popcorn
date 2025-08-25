import { Link } from "react-router";

export function NotFound() {
  return (
    <div className="bg-light-20 absolute top-0 left-0 -z-10 flex min-h-screen w-full flex-col items-center justify-center p-6 text-center">
      <h1 className="font-handjet mb-2 text-9xl font-bold text-orange-100">
        404
      </h1>
      <h2 className="font-inter text-brown-header mb-4 text-3xl font-bold">
        Page Not Found
      </h2>
      <p className="text-brown-60 font-inter mb-8 max-w-md text-lg">
        Oops! Looks like this page has popped away like the last kernel of
        popcorn.
      </p>
      <Link
        to="/introduction"
        className="font-inter rounded-lg bg-orange-100 px-6 py-3 text-white transition-opacity hover:opacity-90"
      >
        Return Home
      </Link>
    </div>
  );
}
