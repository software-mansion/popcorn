defmodule Eval do
  def start() do
    :erlang.process_flag(:trace_calls, true)

    code = read_code()
    result = eval(code)

    result_str = :io_lib.format(~c"eval result: ~p\n", [result])
    :console.print(result_str)

    write_result(result)
  end

  def eval(code) do
    try do
      code = :erlang.binary_to_list(code)
      {:ok, tokens, _} = :erl_scan.string(code)
      Console.print("Scanned\n")
      {:ok, exprs} = :erl_parse.parse_exprs(tokens)
      Console.print("Parsed\n")

      case :erl_eval.exprs(exprs, []) do
        {:value, value, _new_bindings} -> {:ok, value}
        other -> {:error, other}
      end
    catch
      error -> {:error, error}
    end
  end

  defp read_code() do
    {:ok, fd} = :atomvm.posix_open(~c"tmp/code.erl", [:o_rdonly])
    {:ok, code} = :atomvm.posix_read(fd, 1_000_000)
    code
  end

  defp write_result(result) do
    result_bin = :erlang.term_to_binary(result)

    {:ok, fd} =
      :atomvm.posix_open(~c"tmp/result.bin", [:o_creat, :o_wronly], String.to_integer("644", 8))

    {:ok, _size} = :atomvm.posix_write(fd, result_bin)
    :ok
  end
end
