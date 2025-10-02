import XXH from "xxhashjs";

const SEED = 0x42;
const H = XXH.h32(SEED);

export function hashPathname(pathname: string): string {
  return H.update(pathname).digest().toString(16);
}
