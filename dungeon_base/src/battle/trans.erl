-module(trans).

-export([trans/2, trans/3, apply/5]).

-define(MAX_LIMIT, 120).


bin(GivenVal, [Bin|Bins]) -> bin(GivenVal, Bin, Bins, 1).

bin(_, _, [], Ith) -> Ith;
bin(GivenVal, Accum, _, Ith) when GivenVal < Accum -> Ith;
bin(GivenVal, Accum, [Bin|Bins], Ith) -> bin(GivenVal, Accum+Bin, Bins, Ith+1).

% 计算转盘
roulette(AttackSpec,
        #{attr:=#{hit:={single, Hit}, critical:={single, Crit}}},
        #{attr:=#{resist:={single, Res}, block:={single, Blo}, dodge:={single, Dod}}} )->

    % 获得攻击属性（魔法／物理），放招类型（普攻／技能），是否可以抵抗，是否护甲减免（不考虑），技能失败概率
    {AttrType, MoveType, Resistable, _Absorbable, FL}  = AttackSpec,

    % 实际的抗性：如果技能不可抵抗，那么实际的魔抗值为0
    ActualRes = case Resistable of
        resistable -> Res;
        _ -> 0
    end,

    % 实际的闪避：原始的闪避减去了命中加成，直到减到0为止
    ActualDod = case Dod - Hit > 0 of
        true -> Dod - Hit;
        _ -> 0
    end,

    % 实际的失败概率：最大为0.999，最小为0
    ActualFL = if
        FL >= 0.999 -> 0.999;
        FL =< 0 -> 0;
        true -> FL
    end,

    % 通过攻击属性和放招类型，获得最终的闪避／抗性／格挡／暴击／失败概率
    {Dodge, Resist, Block, Critical, FailureRate} = case {AttrType, MoveType} of

        {magic, attack}    -> {0, ActualRes, 0, Crit, ActualFL};

        {physical, attack} -> {ActualDod, 0, Blo, Crit, ActualFL};

        {magic, cast}      -> {0, ActualRes, 0, 0, ActualFL};

        {physical, cast}   -> {0, 0, 0, Crit, ActualFL};

        _ -> {0, 0, 0, 0, 0}

    end,

    % 生成轮盘：
    % 1) 计算出普通攻击／技能的区间
    % 2) 把其它结果加入
    % 3) 按照失败概率稀释原有轮盘，加入失败结果的区间

    Normal = ?MAX_LIMIT - Dodge - Resist - Block - Critical,
    Roulette = [Normal, Dodge, Resist, Block, Critical],
    RouletteWithFailure = [FailureRate/(1-FailureRate) * ?MAX_LIMIT | Roulette],

    % 抽随机数
    Binned = bin(rand:uniform() * ?MAX_LIMIT / (1 - FailureRate), RouletteWithFailure),

    % 得到结果，如果是技能就是cast，平砍是attack
    Result = element(Binned, {failed, MoveType, dodged, resisted, blocked, critical}),
    Result.


repose(#{state:=#{pos:={single, PosO}}=StateO,
         attr:=#{outcome:={single, Outcome}}, range_type:=RangeType} = O,
       #{state:=#{pos:={single, PosD}, hp:={single, HPD}}=StateD,
         attr:=#{is_frozen:={single, IsFrozen}, is_disarmed:={single, IsDisarmed}, is_stunned:={single, IsStunned}}} = D) ->

    % 根据近战远战类型决定追逃动作
    {NewPosO, NewPosD, NewPosMoveO, NewPosMoveD} = case RangeType of

        % 只有当PosO + PosD == 5 的时候才是格斗距离，比这个小说明远了
        near when (PosO + PosD) < 5 ->
            {5 - PosD, PosD, chase, not_assigned_yet};

        % 如果不是，则说明正是格斗距离，不动
        near ->
            {PosO, PosD, stand, not_assigned_yet};

        % 只有当 4 >= PosO + PosD >= 3的时候才是远战格斗距离，比这个再远需要追上
        far when (PosO + PosD) < 3 ->
            {PosO + 1, PosD, chase, not_assigned_yet};

        % 如果是近战的距离需要跳开
        far when PosO + PosD == 5 ->
            case PosO of
                1 -> {PosO, PosD - 2, stand, back_jump_2};
                _ -> {PosO - 1, PosD, back_jump, not_assigned_yet}
            end;

        far ->
            {PosO, PosD, stand, not_assigned_yet}

    end,

    % 决定击飞动作
    BlownRand = rand:uniform(),

    % 如果抽中随机数，且被攻击者不在版边，并且被攻击者不处在冰冻/眩晕/缴械状态，并且被攻击者的反应不是
    {NewPosD2, NewPosMoveD2} = case {NewPosD, NewPosMoveD} of
        {1, not_assigned_yet} -> {1, stand};
        {_, not_assigned_yet} when
            (BlownRand > 0.9) and (IsFrozen == 0) and (IsDisarmed == 0) and (IsStunned == 0)
            and (Outcome /= dodged) and (Outcome /=blocked) and (Outcome /= resisted) or (HPD =< 0) ->
            {NewPosD - 1, blown_out};
        _ -> {NewPosD, stand}
    end,

    % {NewPosO, NewPosD2, NewPosMoveO, NewPosMoveD2},
    {O#{state:=StateO#{pos:={single, NewPosO}, pos_move:={single, NewPosMoveO}}},
     D#{state:=StateD#{pos:={single, NewPosD2}, pos_move:={single, NewPosMoveD2}}}}.



% ========================= TRANSFER INSTRUCTIONS ==============================
% Apply transfer operations over specific attributes of player context. The type
% could be varying state (hp, remaining moves) or attribute (hit, dodge, block,
% etc.) that resets for every round. The supported operations include get, set,
% add, add & multiply original value, or the value referring to other attributes.
%
% trans cares if the damage will be absorbed by the armor of defender.
%
% expecting {Opcode, Value, React} where opcode of set/add/add_mul/add_inc_mul,
% and Value of number, interval or {type, attribute, off/def} triple.


trans({set, Imm, _, _}, Ref) ->
	ref:set(Ref, Imm);

%% 当Inc < 0，且作用的属性是HP时，即是造成伤害，进入伤害处理程序
trans({add, Damage, {_, _, _, Absorbable, _}, Outcome}, {attr, state, hp, P}=ToWhom) when Damage < 0 ->

    %% 处理护甲减免
    AbsorbedDamage = case Absorbable of
        absorbable ->
            erlang:display(ref:val({attr, attr, armor, P})),
            ArmorRatio = 1 - ref:val({attr, attr, armor, P}) * 0.0001,
            Damage * ArmorRatio;
        _ ->
            Damage
        end,

    % erlang:display({absorbed, Absorbable, AbsorbedDamage}),
    %% 处理转盘结果

    CalculatedDamage = case Outcome of
        critical ->
            CritMult = ref:val({attr, attr, critical_multiplier, P}),
            AbsorbedDamage * CritMult;
        attack ->
            AbsorbedDamage;
        cast ->
            AbsorbedDamage;
        resisted ->
            AbsorbedDamage / 10 * rand:uniform();
        _ -> 0
    end,

    FinalDamage = CalculatedDamage * ref:val({attr, attr, damage_multiplier, P}),
    trans({set, ref:val(ToWhom) + FinalDamage, none, none}, ToWhom);

trans({add, Inc, _, _Outcome}, ToWhom) ->
    trans({set, ref:val(ToWhom) + Inc, none, none}, ToWhom);

trans({add_mul, Mul, AttackSpec, Outcome}, ToWhom) ->
    trans({add, ref:val(ToWhom) * Mul, AttackSpec, Outcome}, ToWhom);

trans({add_inc_mul, {Inc, Mul}, AttackSpec, Outcome}, ToWhom) ->
    trans({add, Inc * Mul, AttackSpec, Outcome}, ToWhom).


trans({{Opcode, Oper, AttackSpec}, {attr, Type, Attr, P}}, O, D) ->

    % 获得双方的操作数
    RefOperand = case Oper of
        {Ref} -> ref:val(Ref, O, D);
        {Ref1, Ref2} -> {ref:val(Ref1, O, D), ref:val(Ref2, O, D)}
    end,

    % 得到转盘结果
    Outcome = roulette(AttackSpec, O, D),

    % 将转盘结果加入玩家context，并按结果计算伤害／技能效果，把结果保存在TransPsn里面
    Psn = ref:who(P, O, D),
    TransPsn = trans:trans({Opcode, RefOperand, AttackSpec, Outcome}, {attr, Type, Attr, Psn}),

    {#{attr:=AttrO}=TransO, TransD} = case P of
        off ->  {TransPsn, D};
        def ->  {O, TransPsn}
    end,

    {PosedO, PosedD} = repose(TransO#{attr:=AttrO#{outcome:={single, Outcome}}}, TransD),
    {PosedO, PosedD}.



% ======================== APPLY ALL TRANSFERS IN A LIST ========================
% For each trans operation, apply_trans_with_log combines the player context with
% log. Since the transfers are written in a list, the apply_transes.g_nested will
% apply all the transfers sequentially over the player context, and returns log.

% Accepts cond description

log_trans(#{offender:=Off} = S, SkillName, {_, {_, Type, Attr, Who}},
    #{id:=OID, class:=ClassO, player_name:=NameO, state:=#{hp:={_, HPO}, pos:={_, PosO}, pos_move:={_, PosMoveO}}, attr:=#{outcome:={_, Outcome}}} = O,
    #{id:=DID, class:=ClassD, player_name:=NameD, state:=#{hp:={_, HPD}, pos:={_, PosD}, pos_move:={_, PosMoveD}}} = D
) ->

    {OrderO, OrderD} = case Off == OID of
        true -> {off, def};
        _ -> {def, off}
    end,

    erlang:display({player, NameO, order, OrderO, skill, SkillName, outcome, Outcome}),
    erlang:display({player, NameD, order, OrderD}),

    #{
        state => maps:remove(offender, S),
        effect => #{skill_name=>SkillName, outcome => Outcome, attr=> Attr, over=>Who, diff => ref:val({attr, Type, diff, Who}, O, D)},
        OID => #{player_name=>NameO, class=>ClassO, role=>OrderO, hp=>HPO, pos=>PosO, pos_move=>PosMoveO},
        DID => #{player_name=>NameD, class=>ClassD, role=>OrderD, hp=>HPD, pos=>PosD, pos_move=>PosMoveD}
    }.

apply(S, SkillName, TransList, O, D) ->
    apply(S, SkillName, TransList, O, D, []).

apply(_S, _SkillName, _, #{state:=#{hp:=HPO}}=O, #{state:=#{hp:=HPD}}=D, Logs) when (HPO =<0) or (HPD =< 0) ->
    {O, D, Logs};

apply(_S, _SkillName, [], O, D, Logs) ->
    {O, D, Logs};

apply(S, SkillName, [Trans | RemTrans], O, D, Logs) ->

    % erlang:display({Name, Trans}),
    { TransedO, TransedD} = trans(Trans, O, D),
    Log = log_trans(S, SkillName, Trans, TransedO, TransedD),

    apply(S, SkillName, RemTrans, TransedO, TransedD, [Log | Logs]).
