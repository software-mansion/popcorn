defmodule DemoApp.PatternMatcher do
  @moduledoc false
  # Function-head pattern: :head_pattern_only is a pattern, never in a body.
  def check(:head_pattern_only), do: :head_result
  def check(_), do: :other_result

  # Function-head guard: :head_guard_only is in a guard, never in a body.
  def guarded(x) when x == :head_guard_only, do: :guarded_result
  def guarded(_), do: :unguarded_result

  # Explicit case in the body: :case_pattern_only is a case-clause pattern.
  def classify(result) do
    case result do
      :case_pattern_only -> :classified
      _ -> :unclassified
    end
  end

  # Guard on a case clause in the body: :case_guard_only is a case-clause guard.
  def filter(x) do
    case x do
      v when v == :case_guard_only -> :filtered
      _ -> :passed
    end
  end
end
