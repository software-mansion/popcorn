# Sharing values across languages

Popcorn converts values when they cross between JavaScript and BEAM. The two
directions do not use the same conversion.

Do not assume that a value can make a lossless round trip.

## JavaScript to BEAM

| JavaScript value                                                                                                      | BEAM value             |
| --------------------------------------------------------------------------------------------------------------------- | ---------------------- |
| String                                                                                                                | UTF-8 binary           |
| Boolean                                                                                                               | `true` or `false` atom |
| [Safe integer](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/isSafeInteger) | Integer                |
| Other finite number                                                                                                   | Float                  |
| Array                                                                                                                 | List                   |
| Plain object                                                                                                          | Map with binary keys   |
| `null` or `undefined`                                                                                                 | `nil`                  |
| `atom("ok")`                                                                                                          | Existing atom          |
| `tuple(a, b)`                                                                                                         | Tuple                  |

Buffers and typed-array views become binaries containing their visible bytes.
BigInt becomes an integer. Date becomes a UTC Calendar.ISO DateTime with millisecond precision.
Map becomes a native map. Set becomes MapSet. Symbols use their registry key or description to name existing atoms.

`atom()` also names an existing atom. Unknown atoms and unnamed symbols fail.
Cycles, unsupported instances, functions, unsafe Numbers, and non-finite numbers also fail.

## BEAM to JavaScript

| BEAM value | JavaScript value |
| --- | --- |
| Valid UTF-8 binary | String |
| Other binary | Uint8Array |
| Integer | Number when safe, otherwise BigInt |
| Float | Number |
| List or tuple | Array |
| Map | Plain object |
| DateTime | Date |
| MapSet | Set |
| `true` or `false` | Boolean |
| `nil` or `:null` | `null` |
| Other atom | String |
| PID | Opaque process handle |

Map keys must be atoms, UTF-8 binaries, or numbers with distinct string forms.
Popcorn rejects keys that become equal after conversion. Set members that become
equal can merge, such as `1` and `1n` sent from JavaScript.

DateTimes must use Calendar.ISO and have whole-millisecond precision. Conversion
does not preserve time-zone names, buffer types, object identity, or set order.
PID handles are only usable in the VM instance and boot that created them.

## Tracked JavaScript values

Some JavaScript values cannot become BEAM terms. Examples include DOM nodes,
abort controllers, and library objects.

Return `new TrackedValue(value, cleanup)` from `run_js` to create an opaque
handle. Keep the handle in BEAM state while you need the JavaScript value.

Popcorn runs `cleanup` after the handle is released or the virtual machine
stops. Garbage collection does not provide prompt cleanup. Call an idempotent
cleanup function directly when timing matters.
