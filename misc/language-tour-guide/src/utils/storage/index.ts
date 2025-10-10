import XXH from "xxhashjs";

const SEED = 0x42;
const H32 = XXH.h32(SEED);
const H64 = XXH.h64(SEED);

export function hash32(pathname: string): string {
  return H32.update(pathname).digest().toString(16);
}

export function hash64(code: string): string {
  return H64.update(code).digest().toString(16);
}
