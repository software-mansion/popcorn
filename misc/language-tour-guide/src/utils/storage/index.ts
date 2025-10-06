import XXH from "xxhashjs";

const SEED = 0x42;
const H32 = XXH.h32(SEED);
const H64 = XXH.h64(SEED);

export function hashPathname(pathname: string): string {
  return H32.update(pathname).digest().toString(16);
}

export function hashDefaultCode(code: string): string {
  return H64.update(code).digest().toString(16);
}

export function manageDefaultCodeStorage(concatenatedDefaultCodeHash: string) {
  const hash = hashDefaultCode(concatenatedDefaultCodeHash);
  const previousHash = localStorage.getItem("defaultCodeHash");

  if (previousHash === hash) {
    return;
  }

  localStorage.clear();
  localStorage.setItem("defaultCodeHash", hash);
}
