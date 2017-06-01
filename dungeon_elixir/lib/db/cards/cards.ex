defmodule DungeonElixir.Cards do
	use DungeonElixir.Cards.Schema

	schema "cards" do
	    field :card_name,	 :string
	    field :image_name,	 :string
	    field :level,		 :integer
	    field :expi,		 :integer
	    field :stars,		 :integer

	    field :profession, 	 :string
	    field :range_type, 	 :string

	    field :hp,			 :integer
	    field :armor,		 :integer
	    field :agility,		 :integer
	    field :hit,			 :integer
	    field :block,		 :integer
	    field :dodge,		 :integer
	    field :resist,		 :integer
	    field :critical,	 :integer

	    field :atk_type,	 :string
	    field :atk_max,		 :integer
	    field :atk_min,		 :integer

	    field :last_added, 	 :naive_datetime
	    field :last_modified,:naive_datetime

	end

end