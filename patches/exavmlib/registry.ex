defmodule Registry do
  # Patch reason: :ets.select_delete and :ets.match_delete are unimplemented
  # This function is being used for two types of "unregisters":
  # from pid_ets and key_ets. Since the patch has to access the passed pattern,
  # there are separate clauses needed.
  @doc false
  def __unregister__(key_ets, {key, {pid_to_delete, :_}}, 1) do
    :ets.lookup(key_ets, key)
    |> Enum.filter(fn {_key, {pid, _value}} -> pid == pid_to_delete end)
    |> Enum.each(&:ets.delete_object(key_ets, &1))
  end

  def __unregister__(pid_ets, {pid_to_delete, key, _key_ets, :_}, 2) do
    :ets.lookup(pid_ets, key)
    |> Enum.filter(fn {key, {pid, key, _key_ets, _value}} -> pid == pid_to_delete end)
    |> Enum.each(&:ets.delete_object(pid_ets, &1))
  end
end
