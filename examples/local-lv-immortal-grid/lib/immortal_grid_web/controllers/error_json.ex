defmodule ImmortalGridWeb.ErrorJSON do
  def render("404.json", _assigns), do: %{errors: %{detail: "Not Found"}}
  def render("500.json", _assigns), do: %{errors: %{detail: "Internal Server Error"}}
end
