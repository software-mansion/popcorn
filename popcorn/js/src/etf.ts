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
const STRING_EXT = 0x6b;
const NEW_PID_EXT = 0x58;
const NEWER_REFERENCE_EXT = 0x5a;

const UTF8 = new TextEncoder();
// Keeps a leading U+FEFF, which is part of a string rather than a byte order mark
const TEXT = new TextDecoder("utf-8", { fatal: true, ignoreBOM: true });

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
  // Grows as needed. Binaries are copied into it whole, which a number[]
  // would take a push per byte for.
  private output = new Uint8Array(256);
  private length = 0;
  private readonly buffer = new ArrayBuffer(8);
  private readonly view = new DataView(this.buffer);

  public constructor(private readonly mapper: Mapper = (value) => value) {}

  encode(value: unknown): Uint8Array<ArrayBuffer> {
    this.byte(VERSION);
    this.value(value);
    return this.output.slice(0, this.length);
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

  private termBytes(value: unknown): Uint8Array {
    const start = this.length;
    this.value(value);
    return this.takeFrom(start);
  }

  // Removes the output from `start` on, and returns it
  private takeFrom(start: number): Uint8Array {
    const bytes = this.output.slice(start, this.length);
    this.length = start;
    return bytes;
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
    const start = this.length;
    this.byte(MAP_EXT);
    this.uint32(unique.length);
    for (const entry of unique) {
      this.bytes(entry.key);
      this.bytes(entry.value);
    }
    return this.takeFrom(start);
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

  private reserve(size: number): void {
    if (this.length + size <= this.output.length) return;
    const output = new Uint8Array(
      Math.max(this.output.length * 2, this.length + size),
    );
    output.set(this.output.subarray(0, this.length));
    this.output = output;
  }

  private byte(value: number): void {
    this.reserve(1);
    this.output[this.length++] = value;
  }

  private bytes(values: ArrayLike<number>): void {
    this.reserve(values.length);
    if (values instanceof Uint8Array) {
      this.output.set(values, this.length);
    } else {
      for (let index = 0; index < values.length; index++) {
        this.output[this.length + index] = values[index];
      }
    }
    this.length += values.length;
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

function compareBytes(a: Uint8Array, b: Uint8Array, start = 0): number {
  const length = Math.min(a.length, b.length);
  for (let i = start; i < length; i++) {
    if (a[i] !== b[i]) return a[i] - b[i];
  }
  return a.length - b.length;
}

function compareKeys(left: Uint8Array, right: Uint8Array): number {
  const leftTag = left[0];
  const rightTag = right[0];
  const binaryKeys = leftTag === BINARY_EXT && rightTag === BINARY_EXT;
  const offset = binaryKeys ? BINARY_HEADER_SIZE : 0;
  return compareBytes(left, right, offset);
}

export type Decoded = {
  value: unknown;
  /** The buffers of the `Uint8Array`s in the value, which are their own */
  buffers: ArrayBuffer[];
};

// Keys of maps that popcorn.ts takes for markers, so a map like one is escaped
const MARKER_KEYS: unknown[] = ["popcorn_ref", "popcorn_pid", "popcorn_value"];

// Up to this size, ASCII is faster to decode in JS than with a TextDecoder
const SHORT_TEXT = 32;

/**
 * Decodes a term from the VM, in the external term format, to the values
 * popcorn.ts revives:
 *
 * - `true` and `false` to booleans, `nil` and `null` to `null`, other atoms
 *   to strings;
 * - binaries to strings if they're UTF-8, or else, and in `bytes/1`, to
 *   `Uint8Array`s;
 * - integers to numbers, or bigints if they're unsafe;
 * - tuples and lists to arrays, maps to objects, with keys named like
 *   `wasm.erl` names them;
 * - `DateTime`s to `Date`s;
 * - `MapSet`s, pids and tracked values to markers, which popcorn.ts revives.
 *
 * `wasm.erl` checks the term first, so the decoder doesn't handle what it
 * rejects, such as funs or improper lists.
 */
export function decode(bytes: Uint8Array): Decoded {
  return new Decoder(bytes).decode();
}

class Decoder {
  private offset = 0;
  private readonly view: DataView;
  private readonly buffers: ArrayBuffer[] = [];

  public constructor(private readonly bytes: Uint8Array) {
    this.view = new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength);
  }

  decode(): Decoded {
    check(this.byte() === VERSION, "expected the external term format");
    const value = this.value();
    check(this.offset === this.bytes.length, "expected the term to end");
    return { value, buffers: this.buffers };
  }

  private value(): unknown {
    const tag = this.byte();
    switch (tag) {
      case SMALL_INTEGER_EXT:
        return this.byte();
      case INTEGER_EXT:
        return this.view.getInt32(this.advance(4));
      case SMALL_BIG_EXT:
        return this.big(this.byte());
      case LARGE_BIG_EXT:
        return this.big(this.uint32());
      case NEW_FLOAT_EXT:
        return this.view.getFloat64(this.advance(8));
      case SMALL_ATOM_UTF8_EXT:
        return atomValue(this.text(this.byte()));
      case ATOM_UTF8_EXT:
        return atomValue(this.text(this.uint16()));
      case SMALL_TUPLE_EXT:
        return this.tuple(this.byte());
      case LARGE_TUPLE_EXT:
        return this.tuple(this.uint32());
      case NIL_EXT:
        return [];
      case STRING_EXT:
        // A list of integers under 256
        return Array.from(this.take(this.uint16()));
      case LIST_EXT:
        return this.list(this.uint32());
      case BINARY_EXT:
        return this.binary(this.uint32());
      case MAP_EXT:
        return this.map(this.uint32());
      case NEW_PID_EXT:
        return { popcorn_pid: this.pid() };
      default:
        throw new TypeError(`unexpected term tag ${tag}`);
    }
  }

  private big(size: number): number | bigint {
    const negative = this.byte() === 1;
    const digits = this.take(size);
    if (size <= 6) {
      let value = 0;
      for (let i = size - 1; i >= 0; i--) value = value * 256 + digits[i];
      return negative ? -value : value;
    }
    let value = 0n;
    for (let i = size - 1; i >= 0; i--) {
      value = (value << 8n) | BigInt(digits[i]);
    }
    if (negative) value = -value;
    const safe =
      value >= BigInt(Number.MIN_SAFE_INTEGER) &&
      value <= BigInt(Number.MAX_SAFE_INTEGER);
    return safe ? Number(value) : value;
  }

  private binary(size: number): string | Uint8Array {
    const bytes = this.take(size);
    return textOrNull(bytes) ?? this.copy(bytes);
  }

  // A copy of its own, as a view of the message would expose the rest of it
  private copy(bytes: Uint8Array): Uint8Array<ArrayBuffer> {
    const copy = bytes.slice();
    this.buffers.push(copy.buffer);
    return copy;
  }

  private tuple(arity: number): unknown {
    const start = this.offset;
    const first = arity === 2 || arity === 3 ? this.atomName() : null;
    if (
      first === "__wasm_bytes__" &&
      arity === 2 &&
      this.bytes[this.offset] === BINARY_EXT
    ) {
      this.offset += 1;
      return this.copy(this.take(this.uint32()));
    }
    if (first === "__wasm_tracked_value__" && arity === 3) {
      const key = this.value();
      check(typeof key === "number", "expected a tracked value's key");
      this.reference();
      return { popcorn_ref: key };
    }
    this.offset = start;
    const items = new Array<unknown>(arity);
    for (let i = 0; i < arity; i++) items[i] = this.value();
    return items;
  }

  private list(length: number): unknown[] {
    const items = new Array<unknown>(length);
    for (let i = 0; i < length; i++) items[i] = this.value();
    check(this.byte() === NIL_EXT, "expected a proper list");
    return items;
  }

  private map(arity: number): unknown {
    const keys = new Array<string>(arity);
    const values = new Array<unknown>(arity);
    let struct: string | null = null;
    // A `map` key's value, decoded last, as a MapSet's elements are its keys,
    // and atom keys aren't in alphabetical order
    let mapIndex = -1;
    let mapOffset = 0;
    for (let i = 0; i < arity; i++) {
      const atomKey = isAtom(this.bytes[this.offset]);
      keys[i] = this.key();
      if (atomKey && keys[i] === "__struct__") {
        struct = this.atomName();
        values[i] = struct === null ? this.value() : atomValue(struct);
      } else if (atomKey && keys[i] === "map" && mapIndex === -1) {
        mapIndex = i;
        mapOffset = this.offset;
        this.skip();
      } else {
        values[i] = this.value();
      }
    }
    if (mapIndex !== -1) {
      const end = this.offset;
      this.offset = mapOffset;
      if (struct === "Elixir.MapSet") {
        const members = this.mapSetMembers();
        this.offset = end;
        return { popcorn_value: ["set", members] };
      }
      values[mapIndex] = this.value();
      this.offset = end;
    }
    if (struct === "Elixir.DateTime") return dateTime(keys, values);
    return object(keys, values);
  }

  private mapSetMembers(): unknown[] {
    check(this.byte() === MAP_EXT, "expected a MapSet's map");
    const members = new Array<unknown>(this.uint32());
    for (let i = 0; i < members.length; i++) {
      members[i] = this.value();
      this.skip();
    }
    return members;
  }

  private skip(): void {
    const tag = this.byte();
    switch (tag) {
      case SMALL_INTEGER_EXT:
        this.advance(1);
        return;
      case INTEGER_EXT:
        this.advance(4);
        return;
      case SMALL_BIG_EXT:
        this.advance(1 + this.byte());
        return;
      case LARGE_BIG_EXT:
        this.advance(1 + this.uint32());
        return;
      case NEW_FLOAT_EXT:
        this.advance(8);
        return;
      case SMALL_ATOM_UTF8_EXT:
        this.advance(this.byte());
        return;
      case ATOM_UTF8_EXT:
      case STRING_EXT:
        this.advance(this.uint16());
        return;
      case BINARY_EXT:
        this.advance(this.uint32());
        return;
      case NIL_EXT:
        return;
      case SMALL_TUPLE_EXT:
        this.skipTerms(this.byte());
        return;
      case LARGE_TUPLE_EXT:
        this.skipTerms(this.uint32());
        return;
      case LIST_EXT:
        // With its tail
        this.skipTerms(this.uint32() + 1);
        return;
      case MAP_EXT:
        this.skipTerms(2 * this.uint32());
        return;
      case NEW_PID_EXT:
        this.skip();
        this.advance(12);
        return;
      case NEWER_REFERENCE_EXT: {
        const length = this.uint16();
        this.skip();
        this.advance(4 + 4 * length);
        return;
      }
      default:
        throw new TypeError(`unexpected term tag ${tag}`);
    }
  }

  private skipTerms(count: number): void {
    for (let i = 0; i < count; i++) this.skip();
  }

  // Atoms, integers and UTF-8 binaries; `wasm.erl` renames float keys
  private key(): string {
    const name = this.atomName();
    if (name !== null) return name;
    const key = this.value();
    check(
      typeof key === "string" ||
        typeof key === "number" ||
        typeof key === "bigint",
      "expected a key that wasm.erl accepts",
    );
    return String(key);
  }

  // The name of the atom here, which it moves past, or null for other terms
  private atomName(): string | null {
    switch (this.bytes[this.offset]) {
      case SMALL_ATOM_UTF8_EXT:
        this.offset += 1;
        return this.text(this.byte());
      case ATOM_UTF8_EXT:
        this.offset += 1;
        return this.text(this.uint16());
      default:
        return null;
    }
  }

  // As term_to_binary/1 encodes the pid alone, which popcorn.ts sends back
  private pid(): Uint8Array {
    const start = this.offset - 1;
    check(this.atomName() !== null, "expected a pid's node");
    this.advance(12);
    const external = new Uint8Array(1 + this.offset - start);
    external[0] = VERSION;
    external.set(this.bytes.subarray(start, this.offset), 1);
    this.buffers.push(external.buffer);
    return external;
  }

  // Skips a tracked value's resource, which JS finds by its key
  private reference(): void {
    check(this.bytes[this.offset] === NEWER_REFERENCE_EXT, "expected a ref");
    this.skip();
  }

  private text(size: number): string {
    const text = textOrNull(this.take(size));
    check(text !== null, "expected UTF-8");
    return text;
  }

  private take(size: number): Uint8Array {
    const start = this.advance(size);
    return this.bytes.subarray(start, start + size);
  }

  // Moves past `size` bytes, and returns where they start
  private advance(size: number): number {
    const start = this.offset;
    check(start + size <= this.bytes.length, "expected more of the term");
    this.offset = start + size;
    return start;
  }

  private byte(): number {
    return this.bytes[this.advance(1)];
  }

  private uint16(): number {
    return this.view.getUint16(this.advance(2));
  }

  private uint32(): number {
    return this.view.getUint32(this.advance(4));
  }
}

function textOrNull(bytes: Uint8Array): string | null {
  if (bytes.length <= SHORT_TEXT) {
    let ascii = true;
    for (let i = 0; i < bytes.length && ascii; i++) ascii = bytes[i] < 0x80;
    if (ascii) return String.fromCharCode(...bytes);
  }
  try {
    return TEXT.decode(bytes);
  } catch {
    return null;
  }
}

function isAtom(tag: number): boolean {
  return tag === SMALL_ATOM_UTF8_EXT || tag === ATOM_UTF8_EXT;
}

function atomValue(name: string): unknown {
  switch (name) {
    case "true":
      return true;
    case "false":
      return false;
    case "nil":
    case "null":
      return null;
    default:
      return name;
  }
}

function object(keys: string[], values: unknown[]): unknown {
  if (keys.length === 1 && MARKER_KEYS.includes(keys[0])) {
    return { popcorn_value: ["object", [[keys[0], values[0]]]] };
  }
  const object: Record<string, unknown> = {};
  for (let i = 0; i < keys.length; i++) {
    if (keys[i] === "__proto__") {
      // An own property, which assigning it wouldn't make
      Object.defineProperty(object, "__proto__", {
        value: values[i],
        enumerable: true,
        writable: true,
        configurable: true,
      });
    } else {
      object[keys[i]] = values[i];
    }
  }
  return object;
}

// `wasm.erl` checked that it's in the ISO calendar, whole milliseconds and
// the range of a Date
function dateTime(keys: string[], values: unknown[]): Date {
  const field = (name: string): unknown => values[keys.indexOf(name)];
  const number = (name: string): number => {
    const value = field(name);
    check(typeof value === "number", "expected a DateTime");
    return value;
  };
  const microsecond = field("microsecond");
  check(Array.isArray(microsecond), "expected a DateTime");
  const date = new Date(0);
  date.setUTCFullYear(number("year"), number("month") - 1, number("day"));
  date.setUTCHours(
    number("hour"),
    number("minute"),
    number("second"),
    Number(microsecond[0]) / 1000,
  );
  const offset = (number("utc_offset") + number("std_offset")) * 1000;
  return new Date(date.getTime() - offset);
}
