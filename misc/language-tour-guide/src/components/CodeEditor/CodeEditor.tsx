import CodeMirror from "@uiw/react-codemirror";
import { useState, useCallback } from "react";
import { elixir } from "codemirror-lang-elixir";
import { solarizedLight } from "@uiw/codemirror-theme-solarized";

import "./CodeEditor.styles.css";

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

  const onChange = useCallback((val: string) => {
    console.log("val:", val);
    setValue(val);
  }, []);

  return (
    <CodeMirror
      className="h-full rounded-md border min-h-[500px] overflow-x-scroll border-orange-20 scrollbar"
      value={value}
      extensions={[elixir()]}
      onChange={onChange}
      theme={solarizedLight}
    />
  );
}

export default CodeEditor;
