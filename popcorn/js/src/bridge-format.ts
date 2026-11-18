import { check } from "./utils";

// The tags of values in messages from the VM, see `wasm.erl`
const NULL = 0;
const FALSE = 1;
const TRUE = 2;
const NUMBER = 3;
const BIGINT = 4;
const STRING = 5;
const BINARY = 6;
const BYTES = 7;
const ARRAY = 8;
const OBJECT = 9;
const SET = 10;
const DATE = 11;
const PID = 12;
const TRACKED = 13;
const FRAMED = 14;

// Keeps a leading U+FEFF, which is part of a string rather than a byte order mark
const TEXT = new TextDecoder("utf-8", { fatal: true, ignoreBOM: true });
// Up to this size, ASCII is faster to decode in JS than with a TextDecoder
const SHORT_TEXT = 32;

/** Makes the values that only the page has. */
export type Handles = {
  /** From the pid in the external term format */
  pid: (external: Uint8Array<ArrayBuffer>) => unknown;
  trackedValue: (key: number) => unknown;
};

const NO_HANDLES: Handles = {
  pid: () => {
    throw new TypeError("unexpected pid");
  },
  trackedValue: () => {
    throw new TypeError("unexpected tracked value");
  },
};

/**
 * Decodes a value from the VM, which `wasm.erl` encodes:
 *
 * - `nil` and `null` to `null`, `true` and `false` to booleans, other atoms
 *   to strings;
 * - integers to numbers, or bigints if they're unsafe, floats to numbers;
 * - binaries to strings if they're UTF-8, or else, and in `bytes/1`, to
 *   `Uint8Array`s;
 * - lists and tuples to arrays, maps to objects, with keys that `wasm.erl`
 *   names;
 * - `MapSet`s to sets, `DateTime`s to dates;
 * - pids and tracked values with `handles`.
 */
export function decode(bytes: Uint8Array, handles: Handles): unknown {
  const decoder = new Decoder(bytes, handles, false);
  const value = decoder.value();
  decoder.end();
  return value;
}

/**
 * Decodes a message's envelope in the worker, with its data or args left as
 * bytes, for the page to {@link decode}.
 */
export function decodeEnvelope(message: Uint8Array): unknown {
  const decoder = new Decoder(message, NO_HANDLES, true);
  const value = decoder.value();
  decoder.end();
  return value;
}

/**
 * Returns `t` of a `%{"_popcorn" => %{"t" => t}}` message, as `boot.ex` and
 * `wasm_nif.c` send, or null for other messages, which it doesn't decode.
 */
export function controlType(bytes: Uint8Array): string | null {
  try {
    const decoder = new Decoder(bytes, NO_HANDLES, false);
    if (!decoder.isOnlyKey("_popcorn") || !decoder.isOnlyKey("t")) return null;
    const type = decoder.value();
    decoder.end();
    return typeof type === "string" ? type : null;
  } catch {
    return null;
  }
}

class Decoder {
  private offset = 0;
  private readonly view: DataView;

  public constructor(
    private readonly bytes: Uint8Array,
    private readonly handles: Handles,
    // Whether framed values stay bytes
    private readonly keepFramed: boolean,
  ) {
    this.view = new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength);
  }

  public value(): unknown {
    const tag = this.byte();
    switch (tag) {
      case NULL:
        return null;
      case FALSE:
        return false;
      case TRUE:
        return true;
      case NUMBER:
        return this.view.getFloat64(this.advance(8));
      case BIGINT:
        return BigInt(this.text(this.uint32()));
      case STRING:
        return this.text(this.uint32());
      case BINARY: {
        const bytes = this.take(this.uint32());
        return textOrNull(bytes) ?? bytes.slice();
      }
      case BYTES:
        // A copy of its own, as a view would expose the rest of the message
        return this.take(this.uint32()).slice();
      case ARRAY:
        return this.array(this.uint32());
      case OBJECT:
        return this.object(this.uint32());
      case SET:
        return new Set(this.array(this.uint32()));
      case DATE:
        return new Date(this.view.getFloat64(this.advance(8)));
      case PID:
        return this.handles.pid(this.take(this.uint32()).slice());
      case TRACKED:
        return this.handles.trackedValue(this.view.getInt32(this.advance(4)));
      case FRAMED:
        // The rest of the message
        return this.keepFramed
          ? this.take(this.bytes.length - this.offset)
          : this.value();
      default:
        throw new TypeError(`unexpected tag ${tag}`);
    }
  }

  // Moves past an object with only this key, and returns whether it was one
  public isOnlyKey(name: string): boolean {
    return this.byte() === OBJECT && this.uint32() === 1 && this.key() === name;
  }

  public end(): void {
    check(this.offset === this.bytes.length, "expected the value to end");
  }

  private array(count: number): unknown[] {
    const items = new Array<unknown>(count);
    for (let i = 0; i < count; i++) items[i] = this.value();
    return items;
  }

  private object(count: number): Record<string, unknown> {
    const object: Record<string, unknown> = {};
    for (let i = 0; i < count; i++) {
      const key = this.key();
      const value = this.value();
      if (key === "__proto__") {
        // An own property, which assigning it wouldn't make
        Object.defineProperty(object, key, {
          value,
          enumerable: true,
          writable: true,
          configurable: true,
        });
      } else {
        object[key] = value;
      }
    }
    return object;
  }

  private key(): string {
    return this.text(this.uint32());
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
    check(start + size <= this.bytes.length, "expected more of the value");
    this.offset = start + size;
    return start;
  }

  private byte(): number {
    return this.bytes[this.advance(1)];
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
