import CodeMirror from "@uiw/react-codemirror";
import { useState, useCallback } from "react";
import { elixir } from "codemirror-lang-elixir";

function CodeEditor() {
  const [value, setValue] = useState(`
defmodule EmployeeApp do
  use Ecto.Schema
  import Ecto.Changeset

  alias EmployeeApp.Repo

  schema "employees" do
    field :name, :string
    field :email, :string
    field :department, :string
    field :status, :string, default: "active"
    timestamps()
  end

end
    `);

  const onChange = useCallback((val, viewUpdate) => {
    console.log("val:", val);
    setValue(val);
  }, []);

  return (
    <CodeMirror
      value={value}
      height="200px"
      width="50%"
      extensions={[elixir()]}
      onChange={onChange}
    />
  );
}

export default CodeEditor;
