defmodule Popcorn.JSInteropTest do
  use ExUnit.Case

  require Popcorn.Support.AtomVM, as: AtomVM

  @moduletag :tmp_dir
  @moduletag {:skip_target, :unix}

  test "run_js cross-realm checks", %{tmp_dir: tmp_dir} do
    quote do
      Popcorn.Wasm.run_js!(
        """
        ({ args }) => {
          return [args.a instanceof Array && args.b instanceof Object]
        }
        """,
        %{a: [1, 2, 3], b: %{x: 1, y: 2}},
        return: :value
      )
    end
    |> Macro.to_string()
    |> AtomVM.eval(:elixir, run_dir: tmp_dir)
    |> AtomVM.assert_result(true)
  end
end
