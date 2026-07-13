import { check } from "./utils";

// Tar format constants.
const T = {
  /// entire block size (bytes)
  BLK_N: 512,
  /// filename field offset
  NAME_OFF: 0,
  /// name field size (bytes)
  NAME_N: 100,
  /// file size field offset
  SIZE_OFF: 124,
  /// file size field size (bytes). Stored as ASCII octal string.
  SIZE_N: 12,
  /// entry type field offset, 1 byte.
  TYPEFLAG_OFF: 156,
  /// prefix field offset, 1 byte. Used when name is longer than `name` field.
  PREFIX_OFF: 345,
  /// prefix field size (bytes)
  PREFIX_N: 155,
  /// entry type=dir, '5' in ASCII
  TYPE_DIR: 53,
};

type OnDir = (path: string) => void;
type OnFile = (path: string, data: Uint8Array<ArrayBuffer>) => void;

export function extractTar(
  data: Uint8Array,
  onDir: OnDir,
  onFile: OnFile,
): void {
  check(data.length % T.BLK_N === 0, "tar:bad_chunk");
  const decoder = new TextDecoder();
  let offset = 0;

  while (offset + T.BLK_N <= data.length) {
    const header = data.slice(offset, offset + T.BLK_N);
    if (isZeroBlock(header)) break;

    const name = readString(decoder, header, T.NAME_OFF, T.NAME_N);
    const prefix = readString(decoder, header, T.PREFIX_OFF, T.PREFIX_N);
    const fullName = prefix ? `${prefix}/${name}` : name;
    const size = parseOctal(readString(decoder, header, T.SIZE_OFF, T.SIZE_N));
    const type = header[T.TYPEFLAG_OFF];

    offset += T.BLK_N;

    const path = fullName.startsWith("/") ? fullName : `/${fullName}`;
    if (type === T.TYPE_DIR) {
      onDir(path);
    } else if (fullName) {
      const contents = data.slice(offset, offset + size);
      onFile(path, contents);
    }

    offset += Math.ceil(size / T.BLK_N) * T.BLK_N;
  }
}

function isZeroBlock(block: Uint8Array): boolean {
  for (let i = 0; i < block.length; i++) {
    if (block[i] !== 0) return false;
  }
  return true;
}

function readString(
  decoder: TextDecoder,
  data: Uint8Array,
  start: number,
  length: number,
): string {
  check(length > 0, "tar:bad_string");
  let end = start;
  const max = start + length;
  while (end < max && data[end] !== 0) end++;
  if (end === start) return "";
  return decoder.decode(data.slice(start, end));
}

function parseOctal(value: string): number {
  const parsed = parseInt(value, 8);
  check(!Number.isNaN(parsed), "tar:bad_octal");
  return parsed;
}
