defmodule DungeonElixir.Players do
	use DungeonElixir.Players.Schema

	schema "players" do
		field :player_name,	 	:string
		field :image_name,	 	:string
		field :association,	 	:string
		field :expi,	 		:integer
		field :player_level,	:integer
		field :coins,	 		:integer
		field :diamonds,	 	:integer

		field :preset_card,		:binary_id
		field :selected_skills, {:array, :string}

		field :rating, 			:float
		field :ranking, 		:integer

		field :last_login,		:naive_datetime
		field :last_modified,	:naive_datetime
	end

end