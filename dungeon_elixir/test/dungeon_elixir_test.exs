defmodule DungeonElixirTest do
  use ExUnit.Case
 
  test "get role" do
    assert Ref.get_role(:off, 1, 2) == 1
    assert Ref.get_role(:def, 1, 2) == 2
  end

  test "get value" do
  	assert Ref.get({:attr, :attr, :hp, %{:attr => %{hp: {:single, 5}}}}) == 5
  end

  test "set value" do
  	assert Ref.set({:attr, :attr, :hp, %{:attr => %{hp: {:single, 5}, diff: 0} }}, 6) == %{:attr => %{hp: {:single, 6}, diff: {:single, 1}}}
  end

  test "trans set" do
  	attrs = %{

        diff: {:single, 0},
        attack_disabled: {:single, 0},
        cast_disabled: {:single, 0},
        is_frozen: {:single, 0},
        is_stunned: {:single, 0},
        is_disarmed: {:single, 0},
        damage_multiplier: {:single, 1},
        critical_multiplier: {:single, 2},
        damage_addon: {:single, 0},
        damage_taken: {:single, 0},

        atk_type: {:single, :near},
        atk_range: {:range, -270, -230},
        armor: {:single, 4500},
        agility: {:single, 30},
        hit: {:single, 20},
        block: {:single, 30},
        dodge: {:single, 30},
        resist: {:single, 10},
        critical: {:single, 10},

        outcome: {:single, :null}
    }

    context = %{
        state: %{
            hp: {:single, 5000},
            diff: {:single, 0},
        },

        attr: attrs
    }

    transed = Trans.trans({:set, 20, :haha, :haha}, {:attr, :state, :hp, context})
    assert Ref.val({:attr, :state, :hp, transed}) == 20
  end

  
end
