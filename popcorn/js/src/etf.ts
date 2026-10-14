import {
  err as popcornError,
  isUnserializableReason,
  type Result,
  type UnserializableReason,
} from "./errors";
import { check } from "./utils";

const VERSION = 0x83;
const NEW_FLOAT_EXT = 0x46;
const SMALL_INTEGER_EXT = 0x61;
const INTEGER_EXT = 0x62;
const SMALL_TUPLE_EXT = 0x68;
const LARGE_TUPLE_EXT = 0x69;
const NIL_EXT = 0x6a;
const LIST_EXT = 0x6c;
const BINARY_EXT = 0x6d;
const BINARY_HEADER_SIZE = 1 + 4;
const SMALL_BIG_EXT = 0x6e;
const LARGE_BIG_EXT = 0x6f;
const MAP_EXT = 0x74;
const ATOM_UTF8_EXT = 0x76;
const SMALL_ATOM_UTF8_EXT = 0x77;

const UTF8 = new TextEncoder();

class AtomTerm {
  public constructor(public readonly name: string) {}
}

class TupleTerm {
  public constructor(public readonly entries: unknown[]) {}
}

/**
 * Creates a BEAM atom value.
 *
 * The atom must already exist in the receiving VM. Plain strings encode as binaries.
 */
export function atom(name: string): AtomTerm {
  return new AtomTerm(name);
}

/** Alias for {@link atom}. */
export const a = atom;

/**
 * Creates a BEAM tuple.
 *
 * Plain arrays encode as lists.
 */
export function tuple(
  first: unknown,
  second: unknown,
  ...rest: unknown[]
): TupleTerm {
  check(arguments.length > 1, "tuple requires at least two entries");
  return new TupleTerm([first, second, ...rest]);
}

/** Alias for {@link tuple}. */
export const t = tuple;

/** Pre-encoded ETF sub-term bytes (no version prefix), spliced verbatim. */
export class RawTerm {
  public constructor(public readonly bytes: Uint8Array) {}

  /**
   * Wraps a full external term (`term_to_binary` output) as a spliceable
   * sub-term by dropping its leading version byte.
   */
  public static fromExternal(external: Uint8Array): RawTerm {
    if (external[0] !== VERSION) {
      throw new TypeError("expected a version-prefixed ETF external term");
    }
    return new RawTerm(external.subarray(1));
  }
}

export type Mapper = (value: object) => object;

export function encode(
  data: unknown,
  mapper?: Mapper,
): Result<Uint8Array<ArrayBuffer>, "bridge:unserializable"> {
  try {
    return { ok: true, data: new Encoder(mapper).encode(data) };
  } catch (error) {
    let reason: UnserializableReason = "unsupported";
    let part = data;
    if (error instanceof TypeError && isUnserializableReason(error.message)) {
      reason = error.message;
      part = error.cause;
    }
    return {
      ok: false,
      error: popcornError("bridge:unserializable", { data, part, reason }),
    };
  }
}

class Encoder {
  private readonly ancestors = new Set<object>();
  private readonly output: number[] = [];
  private readonly buffer = new ArrayBuffer(8);
  private readonly view = new DataView(this.buffer);

  public constructor(private readonly mapper: Mapper = (value) => value) {}

  encode(value: unknown): Uint8Array<ArrayBuffer> {
    this.byte(VERSION);
    this.value(value);
    return new Uint8Array(this.output);
  }

  private value(value: unknown): void {
    if (value === null || value === undefined) {
      this.atom("nil");
      return;
    }

    switch (typeof value) {
      case "boolean":
        this.atom(value ? "true" : "false");
        return;
      case "string":
        this.binary(value);
        return;
      case "number":
        this.number(value);
        return;
      case "bigint":
        this.integer(value);
        return;
      case "symbol": {
        const name = Symbol.keyFor(value) ?? value.description;
        if (name === undefined) throw err("unsupported", value);
        this.atom(name);
        return;
      }
      case "object":
        this.object(value);
        return;
      default:
        throw err("unsupported", value);
    }
  }

  private number(value: number): void {
    if (!Number.isFinite(value)) {
      throw err("non-finite-float", value);
    }
    if (!Number.isInteger(value)) {
      this.byte(NEW_FLOAT_EXT);
      this.float64(value);
      return;
    }
    if (!Number.isSafeInteger(value)) {
      throw err("lossy-int", value);
    }
    this.integer(BigInt(value));
  }

  private integer(value: bigint): void {
    if (value >= 0n && value < 256n) {
      this.byte(SMALL_INTEGER_EXT);
      this.byte(Number(value));
      return;
    }
    if (value >= -2147483648n && value < 2147483648n) {
      this.byte(INTEGER_EXT);
      this.int32(Number(value));
      return;
    }
    let magnitude = value < 0n ? -value : value;
    const digits: number[] = [];
    while (magnitude > 0n) {
      digits.push(Number(magnitude & 0xffn));
      magnitude >>= 8n;
    }
    this.byte(digits.length < 256 ? SMALL_BIG_EXT : LARGE_BIG_EXT);
    if (digits.length < 256) this.byte(digits.length);
    else this.uint32(digits.length);
    this.byte(value < 0n ? 1 : 0);
    this.bytes(digits);
  }

  private object(rawValue: object): void {
    const value = this.mapper(rawValue);
    if (value instanceof RawTerm) {
      this.bytes(value.bytes);
      return;
    }
    if (value instanceof AtomTerm) {
      this.atom(value.name);
      return;
    }
    if (this.ancestors.has(value)) {
      throw err("cyclic-object", value);
    }
    this.ancestors.add(value);
    try {
      if (value instanceof TupleTerm) {
        this.tuple(value.entries);
        return;
      }
      if (Array.isArray(value)) {
        this.array(value);
        return;
      }

      if (value instanceof ArrayBuffer) {
        const bytes = new Uint8Array(value);
        this.binary(bytes);
        return;
      }
      if (ArrayBuffer.isView(value)) {
        const bytes = new Uint8Array(
          value.buffer,
          value.byteOffset,
          value.byteLength,
        );
        this.binary(bytes);
        return;
      }
      if (value instanceof Date) {
        this.date(value);
        return;
      }
      if (value instanceof Map) {
        this.entries(value.entries(), value);
        return;
      }
      if (value instanceof Set) {
        this.set(value);
        return;
      }

      const prototype = Object.getPrototypeOf(value);
      const isObject = prototype === Object.prototype || prototype === null;
      if (!isObject) {
        throw err("non-plain-object", value);
      }
      this.entries(Object.entries(value), value);
    } finally {
      this.ancestors.delete(value);
    }
  }

  private tuple(entries: unknown[]): void {
    if (entries.length < 2 ** 8) {
      this.byte(SMALL_TUPLE_EXT);
      this.byte(entries.length);
    } else {
      this.byte(LARGE_TUPLE_EXT);
      this.uint32(entries.length);
    }
    for (const entry of entries) {
      this.value(entry);
    }
  }

  private array(value: unknown[]): void {
    if (value.length === 0) {
      this.byte(NIL_EXT);
      return;
    }

    this.byte(LIST_EXT);
    this.uint32(value.length);
    for (const item of value) {
      this.value(item);
    }
    this.byte(NIL_EXT);
  }

  private set(value: Set<unknown>): void {
    // Elixir 1.20 uses the version-2 Erlang sets map representation.
    const members: [unknown, unknown][] = [];
    for (const member of value) members.push([member, []]);
    const memberBytes = this.mapBytes(members, value, true);
    const map = new RawTerm(memberBytes);
    const fields: [unknown, unknown][] = [
      [atom("__struct__"), atom("Elixir.MapSet")],
      [atom("map"), map],
    ];
    this.entries(fields, value);
  }

  private date(value: Date): void {
    const year = value.getUTCFullYear();
    const validDate = Number.isFinite(value.getTime());
    const supportedYear = year >= -9999 && year <= 9999;
    if (!validDate || !supportedYear) {
      throw err("unsupported", value);
    }
    const fields = {
      __struct__: atom("Elixir.DateTime"),
      calendar: atom("Elixir.Calendar.ISO"),
      year,
      month: value.getUTCMonth() + 1,
      day: value.getUTCDate(),
      hour: value.getUTCHours(),
      minute: value.getUTCMinutes(),
      second: value.getUTCSeconds(),
      microsecond: tuple(value.getUTCMilliseconds() * 1000, 3),
      time_zone: "Etc/UTC",
      zone_abbr: "UTC",
      utc_offset: 0,
      std_offset: 0,
    };
    const entries: [unknown, unknown][] = [];
    for (const [key, item] of Object.entries(fields)) {
      entries.push([atom(key), item]);
    }
    this.entries(entries, value);
  }

  private termBytes(value: unknown): number[] {
    const start = this.output.length;
    this.value(value);
    return this.output.splice(start);
  }

  private mapBytes(
    entries: Iterable<readonly [unknown, unknown]>,
    source: object,
    merge = false,
  ): Uint8Array {
    const encoded = [...entries].map(([key, value]) => ({
      key: this.termBytes(key),
      value: this.termBytes(value),
    }));
    encoded.sort((left, right) => compareKeys(left.key, right.key));
    const unique: typeof encoded = [];
    for (const entry of encoded) {
      const previous = unique.at(-1);
      const duplicate =
        previous !== undefined && compareBytes(entry.key, previous.key) === 0;
      if (duplicate) {
        if (!merge) throw err("unsupported", source);
        continue;
      }
      unique.push(entry);
    }
    const start = this.output.length;
    this.byte(MAP_EXT);
    this.uint32(unique.length);
    for (const entry of unique) {
      this.bytes(entry.key);
      this.bytes(entry.value);
    }
    return new Uint8Array(this.output.splice(start));
  }

  private entries(
    entries: Iterable<readonly [unknown, unknown]>,
    source: object,
  ): void {
    this.bytes(this.mapBytes(entries, source));
  }

  private atom(atom: string): void {
    const bytes = UTF8.encode(atom);
    if ([...atom].length > 255) {
      throw err("unsupported", atom);
    }
    if (bytes.length < 2 ** 8) {
      this.byte(SMALL_ATOM_UTF8_EXT);
      this.byte(bytes.length);
    } else {
      this.byte(ATOM_UTF8_EXT);
      this.uint16(bytes.length);
    }
    this.bytes(bytes);
  }

  private binary(value: string | Uint8Array): void {
    const bytes = typeof value === "string" ? UTF8.encode(value) : value;
    this.byte(BINARY_EXT);
    this.uint32(bytes.length);
    this.bytes(bytes);
  }

  private byte(value: number): void {
    this.output.push(value);
  }

  private bytes(values: ArrayLike<number>): void {
    for (let index = 0; index < values.length; index++) {
      this.output.push(values[index]);
    }
  }

  private uint32(value: number): void {
    this.view.setUint32(0, value);
    this.bytes(new Uint8Array(this.buffer, 0, 4));
  }

  private uint16(value: number): void {
    this.view.setUint16(0, value);
    this.bytes(new Uint8Array(this.buffer, 0, 2));
  }

  private int32(value: number): void {
    this.view.setInt32(0, value);
    this.bytes(new Uint8Array(this.buffer, 0, 4));
  }

  private float64(value: number): void {
    this.view.setFloat64(0, value);
    this.bytes(new Uint8Array(this.buffer));
  }
}

function err(reason: UnserializableReason, part: unknown): TypeError {
  return new TypeError(reason, { cause: part });
}

function compareBytes(a: number[], b: number[], start = 0): number {
  const length = Math.min(a.length, b.length);
  for (let i = start; i < length; i++) {
    if (a[i] !== b[i]) return a[i] - b[i];
  }
  return a.length - b.length;
}

function compareKeys(left: number[], right: number[]): number {
  const leftTag = left[0];
  const rightTag = right[0];
  const binaryKeys = leftTag === BINARY_EXT && rightTag === BINARY_EXT;
  const offset = binaryKeys ? BINARY_HEADER_SIZE : 0;
  return compareBytes(left, right, offset);
}
