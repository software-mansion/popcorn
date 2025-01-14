# Erlfuzz

`erlfuzz` is a small standalone generator of random erlang programs used to fuzz the erlang compiler and VM (and other tools such as erlfmt, dialyzer, eqWAlizer, etc.). It does not currently use coverage information or do anything especially clever, except for carefully handling Erlang's scoping rules.

## Requirements

Dependencies of the fuzzer itself:
- [Rust (including cargo)](https://www.rust-lang.org/tools/install)
- Several libraries listed in `Cargo.toml`

Dependencies of the scripts wrapping fuzzing targets:
- Bash
- GNU Parallel
- Timeout (part of GNU coreutils)

The fuzzer has been tested on both Linux and MacOS.
On the latter, the scripts need a small tweak to run: removing or replacing calls to the `ulimit` command.

## How to build

On a machine with access to the internet: `cargo build --release`

## How to use

### With a provided script

If the goal is to fuzz `erlc` & `erl`, the easiest way is to use the provided `run_erl_debug.sh` script from `erlfuzz/` as follows:
```
mkdir -p out
mkdir -p interesting
seq 1000000 | parallel --line-buffer "./target/release/erlfuzz fuzz -c ./scripts/run_erl_debug.sh --tmp-directory out --interesting-directory interesting test{}"
```
This will repeatedly run the fuzzer, store its output in a file in `out/`, call `erlc` and `erl` on that file, and then either delete it or move it to `interesting/` if it triggered a crash.
It will automatically make use of all cores on your machine. If that is not what you want, you can limit it to 5 cores (for example) by passing `-j 5` to parallel.
It will automatically stop after 1M iterations, just increase the parameter to `seq` if you want more.

I recommend running this in a window controlled by `tmux`, `screen`, etc., so it can be detached and survive the end of whatever terminal/ssh session you are using.

There are similar scripts for fuzzing other tools such as `erlfmt`, `dialyzer`, or `eqwalizer`.
For some of these, it may be required to pass additional options to `erlfuzz`:
- `run_eqwalizer_once.sh`:
   - `--disable-shadowing`
- `run_gradualizer_once.sh`:
  - `--disable-map-comprehensions`
  - `--disable-maybe`
- `run_infer_once.sh`:
   - `--disable-map-comprehensions`
   - `--disable-maybe`
   - `--disable-shadowing`
   - `--wrapper for-infer`
- `verify_erl_jit.sh`:
   - `--deterministic`
   - `--wrapper printing`
- `verify_erlc_opts.sh`:
   - `--deterministic`
   - `--wrapper printing`
- `verify_infer_against_erl.sh`:
   - `--deterministic`
   - `--disable-map-comprehensions`
   - `--disable-maybe`
   - `--disable-shadowing`
   - `--wrapper printing`

### With another script

`erlang fuzz` can be used to fuzz any other command that accepts an Erlang file.
A return code of 0 must be used to indicate that the program ran successfully, and non-0 to report that a bug was found.
I strongly recommend wrapping the program being fuzzed by a script that uses `timeout` and `ulimit -m`; you can look at `run_erl_debug.sh` for inspiration.
Also consider using stronger sandboxing (e.g. with cgroups) if the program under test is likely to have dangerous side effects.

### Manually

Instead of using `erlfuzz fuzz` on a script like above, you can use erlfuzz to generate an Erlang file on stdout, which you can then use however you want. See `erlfuzz generate` for details.

## Testcase minimization

Testcases found by erlfuzz can be automatically reduced, by using `erlfuzz reduce`, and passing it the seed used to generate the testcase.
If any additional option was passed during generation time, it must also be passed at reduction time.
Both the seed and all relevant options are listed in a comment at the top of the testcase.

For example:
```
./target/release/erlfuzz reduce -c ./scripts/run_erl_debug.sh --tmp-directory out --minimized-directory minimized --seed 3137934125722527840 foobar
```
Would reduce the testcase that was generated with seed 3137934125722527840, use `out/foobar.erl` as a scratch file, and store the result in `minimized/foobar.erl`.

Depending on the formatting of the error messages that are output by the tool under test, it can be necessary to adjust the similarity heuristic with the `--max-distance` or the `--num-lines` options. Dialyzer for example includes non-deterministic numbers (timestamp, process id) in its error messages, so `--max-distance 10` is required for erlfuzz to ignore those.

You can ask erlfuzz to automatically reduce the bugs it finds as they are found, by using `erlfuzz fuzz-and-reduce`.
This is a tiny wrapper around doing `erlfuzz fuzz` followed by `erlfuzz reduce`, and needs all of the same arguments.

## Debugging erlfuzz

If erlfuzz itself is misbehaving, one way to investigate is to set the environment variable RUST_LOG=debug or RUST_LOG=trace (even more verbose) to make it emit a log of its actions to stderr.
It is possible to set different modules to different levels of logging, for example RUST_LOG="warn,erlfuzz::reducer=info" will set all modules to level "warn" except for the testcase reducer which it will set to level info. See [crate env_logger](https://docs.rs/env_logger/latest/env_logger/) for details.
The levels from least to most verbose are:
- error (default)
- warn
- info
- debug
- trace

I recommend the following setting to get a log of progress during reduction: `RUST_LOG="debug,erlfuzz::environment=warn"`.

## License

erlfuzz is [Apache licensed](./LICENSE).

## Current limitations

None of the following is currently generated by the fuzzer:
- `receive` constructs (and more generally the code generated is currently non-concurrent)
- preprocessor commands
- map iterators

Erlfuzz also only knows about some bifs.

## Some of the bugs found so far

BEAM VM:
- https://github.com/erlang/otp/issues/6634
- https://github.com/erlang/otp/issues/6645
- https://github.com/erlang/otp/issues/6655
- https://github.com/erlang/otp/issues/6701
- https://github.com/erlang/otp/issues/6717
- https://github.com/erlang/otp/pull/6838
- https://github.com/erlang/otp/pull/6839
- https://github.com/erlang/otp/issues/7282

erlc:
- https://github.com/erlang/otp/issues/6163
- https://github.com/erlang/otp/issues/6164
- https://github.com/erlang/otp/issues/6169
- https://github.com/erlang/otp/issues/6183
- https://github.com/erlang/otp/issues/6184
- https://github.com/erlang/otp/issues/6409
- https://github.com/erlang/otp/issues/6410
- https://github.com/erlang/otp/issues/6418
- https://github.com/erlang/otp/issues/6426
- https://github.com/erlang/otp/issues/6427
- https://github.com/erlang/otp/issues/6444
- https://github.com/erlang/otp/issues/6445
- https://github.com/erlang/otp/issues/6458
- https://github.com/erlang/otp/pull/6415#issuecomment-1312012344
- https://github.com/erlang/otp/issues/6467
- https://github.com/erlang/otp/issues/6468
- https://github.com/erlang/otp/pull/6415#issuecomment-1314007288
- https://github.com/erlang/otp/pull/6415#issuecomment-1315158928
- https://github.com/erlang/otp/issues/6474
- https://github.com/erlang/otp/pull/6415#issuecomment-1318539291
- https://github.com/erlang/otp/pull/6415#issuecomment-1325303532
- https://github.com/erlang/otp/pull/6415#issuecomment-1326240668
- https://github.com/erlang/otp/issues/6458#issuecomment-1326618218
- https://github.com/erlang/otp/issues/6515
- https://github.com/erlang/otp/issues/6551
- https://github.com/erlang/otp/issues/6552
- https://github.com/erlang/otp/issues/6553
- https://github.com/erlang/otp/issues/6552#issuecomment-1353135450
- https://github.com/erlang/otp/pull/6559#issuecomment-1354514130
- https://github.com/erlang/otp/issues/6568
- https://github.com/erlang/otp/issues/6571
- https://github.com/erlang/otp/issues/6572
- https://github.com/erlang/otp/issues/6593
- https://github.com/erlang/otp/issues/6599
- https://github.com/erlang/otp/issues/6601
- https://github.com/erlang/otp/issues/6602
- https://github.com/erlang/otp/issues/6603
- https://github.com/erlang/otp/issues/6604
- https://github.com/erlang/otp/issues/6612
- https://github.com/erlang/otp/issues/6613
- https://github.com/erlang/otp/pull/6619#issuecomment-1368995152
- https://github.com/erlang/otp/issues/6630
- https://github.com/erlang/otp/issues/6633
- https://github.com/erlang/otp/issues/6643
- https://github.com/erlang/otp/issues/6648
- https://github.com/erlang/otp/pull/6651#issuecomment-1377177470
- https://github.com/erlang/otp/pull/6651#issuecomment-1377216462
- https://github.com/erlang/otp/issues/6660
- https://github.com/erlang/otp/pull/6727#issuecomment-1418853979
- https://github.com/erlang/otp/issues/6835
- https://github.com/erlang/otp/issues/6847
- https://github.com/erlang/otp/issues/6848
- https://github.com/erlang/otp/issues/6851
- https://github.com/erlang/otp/issues/6960
- https://github.com/erlang/otp/issues/6962
- https://github.com/erlang/otp/pull/6974
- https://github.com/erlang/otp/issues/7011
- https://github.com/erlang/otp/issues/7128
- https://github.com/erlang/otp/issues/7142
- https://github.com/erlang/otp/issues/7145
- https://github.com/erlang/otp/issues/7147
- https://github.com/erlang/otp/issues/7168
- https://github.com/erlang/otp/issues/7170
- https://github.com/erlang/otp/issues/7171
- https://github.com/erlang/otp/issues/7178
- https://github.com/erlang/otp/issues/7179
- https://github.com/erlang/otp/issues/7180
- https://github.com/erlang/otp/pull/7207
- https://github.com/erlang/otp/pull/7222
- https://github.com/erlang/otp/issues/7248
- https://github.com/erlang/otp/issues/7251
- https://github.com/erlang/otp/issues/7252
- https://github.com/erlang/otp/pull/7271
- https://github.com/erlang/otp/pull/7283
- https://github.com/erlang/otp/pull/7281#issuecomment-1562404185
- https://github.com/erlang/otp/pull/7340
- https://github.com/erlang/otp/issues/7354
- https://github.com/erlang/otp/issues/7370
- https://github.com/erlang/otp/pull/7448#issuecomment-1618443949
- https://github.com/erlang/otp/issues/7467
- https://github.com/erlang/otp/issues/7468
- https://github.com/erlang/otp/pull/7448#issuecomment-1621245246
- https://github.com/erlang/otp/issues/7477
- https://github.com/erlang/otp/issues/7478
- https://github.com/erlang/otp/pull/7489#issuecomment-1633795558
- https://github.com/erlang/otp/issues/7494
- https://github.com/erlang/otp/issues/7504
- https://github.com/erlang/otp/pull/7527#issue-1833154980
- https://github.com/erlang/otp/issues/7901
- https://github.com/erlang/otp/pull/7902#issuecomment-1825299276
- https://github.com/erlang/otp/pull/7902#issuecomment-1825301929
- https://github.com/erlang/otp/issues/8097#issuecomment-1933693807
- https://github.com/erlang/otp/pull/8101
- https://github.com/erlang/otp/issues/8190
- https://github.com/erlang/otp/pull/8196#issuecomment-1966811985
- https://github.com/erlang/otp/pull/8196#issuecomment-1968660985

dialyzer:
- https://github.com/erlang/otp/issues/6419
- https://github.com/erlang/otp/issues/6473
- https://github.com/erlang/otp/issues/6518
- https://github.com/erlang/otp/issues/6580
- https://github.com/erlang/otp/issues/7138
- https://github.com/erlang/otp/issues/7153
- https://github.com/erlang/otp/issues/7181
- https://github.com/erlang/otp/issues/7325

erlfmt:
- https://github.com/WhatsApp/erlfmt/issues/338

eqWAlizer:
- https://github.com/WhatsApp/eqwalizer/commit/56634681d5de33819c371285ff5682d58384518b
- https://github.com/WhatsApp/eqwalizer/commit/a253f6ee5c202d683c2719d325fd31acc46221a3
- https://github.com/WhatsApp/eqwalizer/commit/d8afa8e7cc27a115570198539128cffd16e16866
- https://github.com/WhatsApp/eqwalizer/commit/75de28b27345b7796cf4c39a460d93b1070e02ac
- https://github.com/WhatsApp/eqwalizer/commit/ae608820ec08c2108e438a92b9d7a0fdf999a06b
- https://github.com/WhatsApp/eqwalizer/commit/904449753e68388bc5b33f8b12217b6af6978bf7

inferl:
- https://github.com/facebook/infer/commit/dbdcf4863ee2751a6b671f072850b29b4916bf5b
- https://github.com/facebook/infer/commit/5453911ea7a69dfb66f2cb697976eeeb9c30b176

Gradualizer
- https://github.com/josefs/Gradualizer/issues/542
- https://github.com/josefs/Gradualizer/issues/543
- https://github.com/josefs/Gradualizer/issues/544
- https://github.com/josefs/Gradualizer/issues/545
