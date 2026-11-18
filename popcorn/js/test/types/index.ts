import { Popcorn, PopcornError } from "@swmansion/popcorn";
import { popcorn as vite } from "@swmansion/popcorn/vite";
import { popcorn as rollup } from "@swmansion/popcorn/rollup";
import { popcorn as esbuild } from "@swmansion/popcorn/esbuild";

export const initialize: typeof Popcorn.init = Popcorn.init;
export const ErrorClass = PopcornError;
export const plugins = [
  vite({ rootDir: "." }),
  rollup({ rootDir: "." }),
  esbuild({ rootDir: "." }),
];

// @ts-expect-error PopcornError is a constructor.
export const invalidError: symbol = PopcornError;
// @ts-expect-error Initialization options must be recognized.
Popcorn.init({ unknownOption: 123 });
// @ts-expect-error Vite requires a project directory.
vite({});
// @ts-expect-error Rollup requires a project directory.
rollup({});
// @ts-expect-error esbuild requires a project directory.
esbuild({});
