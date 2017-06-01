defmodule Battle do

    def toss(%{selected_skills: [:rune_of_the_void|_], id: a}, _), do: a
    def toss(_, %{selected_skills: [:rune_of_the_void|_], id: b}), do: b

    def toss(%{id: a, attr: %{agility: {:single, agi_a}}},
             %{id: b, attr: %{agility: {:single, agi_b}}}) do
    
        case :rand.uniform() * (agi_a + agi_b) > agi_a do
            true -> b
            _    -> a
        end
    end

    def refresh_attributes(%{orig_attr: orig_a} = player_a, %{orig_attr: orig_b} = player_b) do
        {%{player_a| :attr=>orig_a}, %{player_b| :attr=>orig_b}}
    end
    
    def next_state(%{stage: :casting, seq: seq}=state, player_a, player_b) do
        offender = toss(player_a, player_b)
        :erlang.display({"=============", :begin_settling, seq+1, offender, :first})
        %{state| stage: :settling, seq: seq+1, offender: offender}
    end

    def next_state(%{stage: :settling, seq: seq}=state, _, _) do
        :erlang.display({"=============", :begin_casting, seq})
        %{state| stage: :casting}
    end


    def apply_move_both(%{state: :settling}=state, a, b, log) do
        {trans_a, trans_b, trans_log}    = CastApply.effect(state, a, b, log)                            # a上回合遗留下来的出招前的效果
        {trans_2b, trans_2a, trans_2log} = CastApply.effect(state, trans_b, trans_a, trans_log)          # b上回合遗留下来的出招前效果
        {trans_2a, trans_2b, trans_2log}
    end


    def apply_move_both(%{stage: :casting}=state, a, b, log) do
        {trans_a, trans_b, trans_log}               = CastApply.cast(S, a, b, log)                                                                       #% a出招
        {trans_Effa, trans_Effb, trans_Efflog}      = CastApply.effect(state, trans_a, trans_b, trans_log)                                      #% a技能效果
        {trans_Eff2b, trans_Eff2a, trans_Eff2log}   = CastApply.effect(%{state| :stage=>:counter}, trans_Effb, trans_Effa, trans_Efflog)         #% b的反应技能效果
        {trans_Eff3a, trans_Eff3b, trans_Eff3log}   = CastApply.effect(%{state| :stage=>:append}, trans_Eff2a, trans_Eff2b, trans_Eff2log)       #% a的追加技能效果

        {trans_2b, trans_2a, trans_2log}            = CastApply.cast(state, trans_Eff3b, trans_Eff3a, trans_Eff3log)                                  #% b出招
        {trans_2Effb, trans_2Effa, trans_2Efflog}   = CastApply.effect(state, trans_2b, trans_2a, trans_2log)                                #% b出招效果
        {trans_2Eff2a, trans_2Eff2b, trans_2Eff2log}= CastApply.effect(%{state| :stage=>:counter}, trans_2Effa, trans_2Effb, trans_2Efflog)   #% a的反应出招效果
        {trans_2Eff3b, trans_2Eff3a, trans_2Eff3log}= CastApply.effect(%{state| :stage=>:append}, trans_2Eff2b, trans_2Eff2a, trans_2Eff2log) #% b的追加技能效果

        {refreshed_a, refreshed_b} = refresh_attributes(trans_2Eff3a, trans_2Eff3b)
        {refreshed_a, refreshed_b, trans_2Eff3log}
    end


    def apply_move_ordered(%{offender: offender}=state, %{id: offender}=a, b, log) do
        apply_move_both(state, a, b, log)
    end

    def apply_move_ordered(%{offender: offender}=state, a, %{id: offender}=b, log) do
        {new_b, new_a, new_log} = apply_move_both(state, b, a, log)
        {new_a, new_b, new_log}
    end


    def loop(_, %{selected_skills: selected_a, state: %{hp: {:single, hp_a}}, id: id1}, %{selected_skills: selected_b, state: %{hp: {:single, hp_b}}, id: id2}, log) when hp_a < 0 or hp_b < 0 do

        {winner, loser} = if hp_a > hp_b, do: {id1, id2}, else: {id2, id1}

        :erlang.display({selected_a, selected_b})
        :erlang.display({:ended, :someone_died})
        {log, %{records: :lists.reverse(log), winner: winner, loser: loser}}
    end

    def loop(%{seq: seq}, %{selected_skills: selected_a, state: %{hp: {:single, hp_a}}, id: id1}, %{selected_skills: selected_b, state: %{hp: {:single, hp_b}}, id: id2}, log) when seq > 22 do

        {winner, loser} = if hp_a > hp_b, do: {id1, id2}, else: {id2, id1}

        :erlang.display({selected_a, selected_b})
        :erlang.display({:ended, :no_more_skills})
        {log, %{records: :lists.reverse(log), winner: winner, loser: loser}}
    end

    def loop(state, a, b, log) do

        {applied_a, applied_b, applied_log} = apply_move_ordered(state, a, b, log)

        loop(next_state(state, a, b), applied_a, applied_b, applied_log)
    end

    def start({%{selected_skills: selected_skills_a} = a, %{selected_skills: selected_skills_b} = b}) do

        :erlang.display(:battle_begins)

        state = next_state(%{seq: 0, stage: :casting}, a, b)

        {effects_a, effects_b} = CastGenerate.effects(selected_skills_a, selected_skills_b)

        {casts_a, casts_b} = CastGenerate.casts(effects_a, effects_b)

        :error_logger.info_report(effects_a)
        :error_logger.info_report(effects_b)

        loop(state, %{a| effects: effects_a, casts: casts_a}, %{b| effects: effects_b, casts: casts_b}, [])
    end

end