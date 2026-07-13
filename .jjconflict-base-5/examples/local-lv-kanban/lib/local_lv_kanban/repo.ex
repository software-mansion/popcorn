defmodule LocalLvKanban.Repo do
  use Ecto.Repo,
    otp_app: :local_lv_kanban,
    adapter: Ecto.Adapters.Postgres
end
