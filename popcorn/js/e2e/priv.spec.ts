import { assert, evalOpts, expect, test } from "./helpers";

test("application priv files", async ({ otp }) => {
  const boot = await otp.boot(
    evalOpts(`
      PrivDir = code:priv_dir(test_entrypoint),
      AppPath = 'Elixir.Application':app_dir(test_entrypoint, <<"priv/fixture.txt">>),
      {ok, Contents} = file:read_file(AppPath),
      ok = wasm:send(#{
        priv_dir => list_to_binary(PrivDir),
        app_path => AppPath,
        contents => Contents
      }).
    `),
  );
  assert(boot.ok);

  expect(await otp.waitForEvent("priv_dir")).toEqual({
    priv_dir: "/lib/test_entrypoint/priv",
    app_path: "/lib/test_entrypoint/priv/fixture.txt",
    contents: "packaged application priv\n",
  });
});
