-module(test).

-export([modify_skill/2]).


modify_skill([{prob_group, WhichGroup} | Rem], ProbGroups) ->
    setelement(WhichGroup, modify_skill(Rem, element(WhichGroup, ProbGroups)), ProbGroups);
modify_skill([all_prob_group | Rem], ProbGroups) ->
    ProbGroupsList = tuple_to_list(ProbGroups),
    Applied = [modify_skill(Rem, ProbGroup) || ProbGroup <- ProbGroupsList],
    list_to_tuple(Applied);

modify_skill([{prob, NewProb}], ProbGroup) ->
    setelement(1, NewProb, ProbGroup);
modify_skill([cond_groups | Rem], ProbGroup) ->
    setelement(2, modify_skill(Rem, element(2, ProbGroup)), ProbGroup);

modify_skill([{cond_group, WhichGroup} | Rem], CondGroups) ->
    setelement(WhichGroup, modify_skill(Rem, element(WhichGroup, CondGroups)), CondGroups);
modify_skill([all_cond_group | Rem], CondGroups) ->
    CondGroupsList = tuple_to_list(CondGroups),
    Applied = [modify_skill(Rem, CondGroup) || CondGroup <- CondGroupsList],
    list_to_tuple(Applied);

modify_skill([{cond_, Cond}], CondGroup) ->
    setelement(1, Cond, CondGroup);
modify_skill([trans | Rem], CondGroup) ->
    setelement(2, modify_skill(Rem, element(2, CondGroup)), CondGroup);

modify_skill([{trans_group, WhichGroup} | Rem], Trans) ->
    setelement(WhichGroup, modify_skill(Rem, element(WhichGroup, Trans)), Trans);
modify_skill([all_trans_group | Rem], Trans) ->
    TransGroupsList = tuple_to_list(Trans),
    Applied = [modify_skill(Rem, TransGroup) || TransGroup <- TransGroupsList],
    list_to_tuple(Applied);
modify_skill([{add_trans_group, NewTrans}], Trans) ->
    list_to_tuple([NewTrans | tuple_to_list(Trans)]);

modify_skill([{match_attack_type, AttackType, RefValue} ], Trans) ->
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

test() ->

    Trans = {{add_inc_mul, {{attr, atk_range, off}, {single, -1}}, {physical, attack, absorbable, non_resistable, 0}}, {attr, hp, def}, {chase, blowable}}.
