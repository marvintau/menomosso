defmodule DungeonElixir.PlayerCardInfo do
	use DungeonElixir.PlayerCardInfo.Schema

	schema "player_card_info" do
	    field :card_id,		:binary_id
	    field :player_id, 	:binary_id

	    field :last_added, 		:naive_datetime
	    field :last_modified, 	:naive_datetime
	end


end