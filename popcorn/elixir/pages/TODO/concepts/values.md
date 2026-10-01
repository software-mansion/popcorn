# Values across the bridge

Popcorn converts values when they cross between JavaScript and BEAM. The two
directions do not use the same conversion.

## JavaScript to BEAM

| JavaScript value | BEAM value |
| --- | --- |
| String | UTF-8 binary |
| Boolean | `true` or `false` atom |
| [Safe integer](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/isSafeInteger) | Integer |
| Other finite number | Float |
| Array | List |
| Plain object | Map with binary keys |
| `null` or `undefined` | `nil` |
| `atom("ok")` | Existing atom |
| `tuple(a, b)` | Tuple |

Use `atom()` only for atoms that already exist in the virtual machine. Popcorn
rejects unknown atoms instead of creating them.

Popcorn rejects cyclic objects, class instances, functions, symbols, bigints,
unsafe integers, and non-finite numbers.

## BEAM to JavaScript

Most atoms become strings. The atoms `true` and `false` become booleans.
Tuples become arrays, and maps become objects.

BEAM PIDs become opaque JavaScript values. Use a PID only with the Popcorn
instance and boot that created it.

Do not assume that a value can make a lossless round trip. Define a small wire
shape for each public message.

## Tracked JavaScript values

Some JavaScript values cannot become BEAM terms. Examples include DOM nodes,
abort controllers, and library objects.

Return `new TrackedValue(value, cleanup)` from `run_js` to create an opaque
handle. Keep the handle in BEAM state while you need the JavaScript value.

Popcorn runs `cleanup` after the handle is released or the virtual machine
stops. Garbage collection does not provide prompt cleanup. Call an idempotent
cleanup function directly when timing matters.
