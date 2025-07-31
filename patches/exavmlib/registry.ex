defmodule Registry do
  # Patch reason: :ets.select_delete and :ets.match_delete are unimplemented
  @doc false
  # def __unregister__(table, match, key_pos) do
  #   # This function is being used for two types of "unregisters":
  #   # from pid_ets and key_ets. This handles patterns for both
  #   {key, pid_to_delete} =
  #     case {match, key_pos} do
  #       {{key, {pid, :_}}, 1} -> {key, pid}
  #       {{pid, key, _key_ets, :_}, 2} -> {key, pid}
  #     end
  # end

  def __unregister__(key_ets, {key, {pid_to_delete, :_}}, 1) do
    :ets.lookup(key_ets, key)
    |> Enum.filter(fn {_key, {pid, _value}} -> pid == pid_to_delete end)
    |> Enum.each(&:ets.delete_object(key_ets, &1))
  end

  def __unregister__(pid_ets, {pid_to_delete, key, _key_ets, :_}, 2) do
    :ets.lookup(pid_ets, key)
    |> Enum.filter(fn {_key, {pid, _key, _key_ets, _value}} -> pid == pid_to_delete end)
    |> Enum.each(&:ets.delete_object(pid_ets, &1))
  end
end
