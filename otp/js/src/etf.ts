import {
  err as popcornError,
  isUnserializableReason,
  type Result,
  type UnserializableReason,
} from "./errors";

const VERSION = 0x83;
const NEW_FLOAT_EXT = 0x46;
const SMALL_INTEGER_EXT = 0x61;
const INTEGER_EXT = 0x62;
const SMALL_TUPLE_EXT = 0x68;
const NIL_EXT = 0x6a;
const LIST_EXT = 0x6c;
const BINARY_EXT = 0x6d;
const SMALL_BIG_EXT = 0x6e;
const MAP_EXT = 0x74;
const SMALL_ATOM_UTF8_EXT = 0x77;

const UTF8 = new TextEncoder();

type Atom = "true" | "false" | "nil";

export function tuple2(
  data: unknown,
  meta: unknown,
): Result<Uint8Array<ArrayBuffer>, "bridge:unserializable"> {
  try {
    return { ok: true, data: new Encoder().tuple2(data, meta) };
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

  tuple2(left: unknown, right: unknown): Uint8Array<ArrayBuffer> {
    this.byte(VERSION);
    this.byte(SMALL_TUPLE_EXT);
    this.byte(2);
    this.value(left);
    this.value(right);
    return new Uint8Array(this.output);
  }

  private value(value: unknown): void {
    if (value === null) {
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
    if (value >= 0 && value < 2 ** 8) {
      this.byte(SMALL_INTEGER_EXT);
      this.byte(value);
      return;
    }
    if (value >= -(2 ** 31) && value < 2 ** 31) {
      this.byte(INTEGER_EXT);
      this.int32(value);
      return;
    }

    this.smallBigInt(value);
  }

  // https://www.erlang.org/doc/apps/erts/erl_ext_dist.html#small_big_ext
  private smallBigInt(value: number): void {
    let magnitude = BigInt(Math.abs(value));
    const digits: number[] = [];
    while (magnitude > 0n) {
      digits.push(Number(magnitude & 0xffn));
      magnitude >>= 8n;
    }
    this.byte(SMALL_BIG_EXT);
    this.byte(digits.length);
    this.byte(value < 0 ? 1 : 0);
    this.bytes(digits);
  }

  private object(value: object): void {
    if (this.ancestors.has(value)) {
      throw err("cyclic-object", value);
    }
    this.ancestors.add(value);
    try {
      if (Array.isArray(value)) {
        this.array(value);
        return;
      }

      const prototype = Object.getPrototypeOf(value);
      const isObject = prototype === Object.prototype || prototype === null;
      if (!isObject) {
        throw err("non-plain-object", value);
      }
      this.map(value);
    } finally {
      this.ancestors.delete(value);
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

  private map(value: object): void {
    const keys = Object.keys(value).sort();
    this.byte(MAP_EXT);
    this.uint32(keys.length);
    for (const key of keys) {
      this.binary(key);
      this.value((value as Record<string, unknown>)[key]);
    }
  }

  private atom(atom: Atom): void {
    const bytes = UTF8.encode(atom);
    this.byte(SMALL_ATOM_UTF8_EXT);
    this.byte(bytes.length);
    this.bytes(bytes);
  }

  private binary(value: string): void {
    const bytes = UTF8.encode(value);
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
