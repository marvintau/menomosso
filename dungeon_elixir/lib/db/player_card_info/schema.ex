defmodule DungeonElixir.PlayerCardInfo.Schema do
  defmacro __using__(_) do
    quote do
      use Ecto.Schema
      @primary_key {:entry_id, :binary_id, autogenerate: false}
      @foreign_key_type :binary_id
    end
  end
end