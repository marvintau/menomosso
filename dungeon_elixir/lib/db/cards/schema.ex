defmodule DungeonElixir.Cards.Schema do
  defmacro __using__(_) do
    quote do
      use Ecto.Schema
      @primary_key {:card_id, :binary_id, autogenerate: false}
      @foreign_key_type :binary_id
    end
  end
end