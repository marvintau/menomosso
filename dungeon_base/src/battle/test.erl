-module(test).

-export([modify_skill/2, test/0]).


modify_skill([{prob_group, WhichGroup} | Rem], ProbGroups) ->
    setelement(WhichGroup, ProbGroups, modify_skill(Rem, element(WhichGroup, ProbGroups)));
modify_skill([all_prob_group | Rem], ProbGroups) ->
    ProbGroupsList = tuple_to_list(ProbGroups),
    Applied = [modify_skill(Rem, ProbGroup) || ProbGroup <- ProbGroupsList],
    list_to_tuple(Applied);

modify_skill([{prob, NewProb}], ProbGroup) ->
    setelement(1, ProbGroup, NewProb);
modify_skill([cond_groups | Rem], ProbGroup) ->
    setelement(2, ProbGroup, modify_skill(Rem, element(2, ProbGroup)));

modify_skill([{cond_group, WhichGroup} | Rem], CondGroups) ->
    setelement(WhichGroup, CondGroups, modify_skill(Rem, element(WhichGroup, CondGroups)));
modify_skill([all_cond_group | Rem], CondGroups) ->
    CondGroupsList = tuple_to_list(CondGroups),
    Applied = [modify_skill(Rem, CondGroup) || CondGroup <- CondGroupsList],
    list_to_tuple(Applied);

modify_skill([{cond_, Cond}], CondGroup) ->
    setelement(1, CondGroup, Cond);
modify_skill([trans | Rem], CondGroup) ->
    setelement(2, CondGroup, modify_skill(Rem, element(2, CondGroup)));

modify_skill([{trans_group, WhichGroup} | Rem], Trans) ->
    setelement(WhichGroup, Trans, modify_skill(Rem, element(WhichGroup, Trans)));
modify_skill([all_trans_group | Rem], Trans) ->
    TransGroupsList = tuple_to_list(Trans),
    Applied = [modify_skill(Rem, TransGroup) || TransGroup <- TransGroupsList],
    list_to_tuple(Applied);
modify_skill([{add_trans_group, NewTrans}], Trans) ->
    list_to_tuple([NewTrans | tuple_to_list(Trans)]);

modify_skill([{match_attack_type, AttackType, RefValue} ], Trans) ->
    erlang:display(Trans),
    {{Operator, _RefValueTrans, AttackTypeTrans}, AffectedAttr, Pos} = Trans,

    case AttackTypeTrans == AttackType of
        true -> {{Operator, RefValue, AttackType}, AffectedAttr, Pos};
        _ -> Trans
    end;

modify_skill([{match_affected_attr, AffectedAttr, RefValue} ], Trans) ->
    {{Operator, _RefValueTrans, AttackTypeTrans}, AffectedAttrTrans, Pos} = Trans,

    case AffectedAttr == AffectedAttrTrans of
        true -> {{Operator, RefValue, AttackTypeTrans}, AffectedAttr, Pos};
        _ -> Trans
    end.


seq() ->
    seq(0).
seq(LastFor) ->
    seq(LastFor, casting).
seq(LastFor, Stage) ->
    seq(LastFor, Stage, []).
seq(LastFor, Stage, Conds) ->
    {{seq_norm, 0, LastFor, Stage}, Conds}.

physical_attack_spec() ->
    {physical, attack, absorbable, non_resistable, 0}.


plain_attack() ->
    {{add_inc_mul, {{attr, atk_range, off}, {single, -1}}, physical_attack_spec()}, {attr, hp, def}, {chase, blowable}}.


test() ->

    Skill = {{0, {
            {seq(), {plain_attack()}}
        }}},
    
    erlang:display("===============PROB================="),
    erlang:display(modify_skill([all_prob_group, {prob, 1}], Skill)),
    erlang:display("===============PROB================="),
    erlang:display(modify_skill([all_prob_group, cond_groups, all_cond_group, {cond_, seq(1)}], Skill)),
    erlang:display("===============PROB================="),
    erlang:display(modify_skill([all_prob_group, cond_groups, all_cond_group, trans, {add_trans_group, plain_attack()}], Skill)),
    erlang:display("===============PROB================="),
    erlang:display(modify_skill([all_prob_group, cond_groups, all_cond_group, trans, all_trans_group, {match_attack_type, {attr, hp, def}, {{attr, atk_range, off}, {single, -2}}}], Skill)).

