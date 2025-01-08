#
# This file is part of AtomVM.
#
# Copyright 2021 Davide Bettio <davide@uninstall.it>
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#    http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
# SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
#

defmodule Code do
  @compile {:autoload, false}
  @moduledoc """
  This module is to satisfy certain code loading checks in Elixir,
  specifically with regards to protocols support.
  """

  @doc """
  required for protocols to work with Elixir >= 1.16, due to code loading checks.
  """
  def ensure_compiled(module) do
    :code.ensure_loaded(module)
  end

  @doc """
  previously required for protocols support, due to code loading checks.
  """
  @deprecated "Use Code.ensure_compiled/1 instead"
  def ensure_compiled?(module) do
    match?({:module, ^module}, ensure_compiled(module))
  end

  def eval_string(string, binding \\ [], opts \\ [])

  def eval_string(string, binding, %Macro.Env{} = env) do
    validated_eval_string(string, binding, env)
  end

  def eval_string(string, binding, opts) when is_list(opts) do
    validated_eval_string(string, binding, opts)
  end

  defp validated_eval_string(string, binding, opts_or_env) do
    %{line: line, file: file} = env = env_for_eval(opts_or_env)
    forms = :elixir.string_to_quoted!(to_charlist(string), line, 1, file, [])
    IO.inspect({:xddd, forms})
    {value, binding, _env} = eval_verify(:eval_forms, [forms, binding, env])
    {value, binding}
  end

  defp eval_verify(fun, args) do
    Module.ParallelChecker.verify(fn ->
      apply(:elixir, fun, args)
    end)
  end

  def env_for_eval(env_or_opts), do: :elixir.env_for_eval(env_or_opts)
end
