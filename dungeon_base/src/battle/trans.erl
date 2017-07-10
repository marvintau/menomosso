-module(trans).

% -export([trans/2, trans/3]).
-export([effect/4]).
-export([cast/4]).

-define(MAX_LIMIT, 120).


bin(GivenVal, [Bin|Bins]) -> bin(GivenVal, Bin, Bins, 1).

bin(_, _, [], Ith) -> Ith;
bin(GivenVal, Accum, _, Ith) when GivenVal < Accum -> Ith;
bin(GivenVal, Accum, [Bin|Bins], Ith) -> bin(GivenVal, Accum+Bin, Bins, Ith+1).

% 计算转盘
roulette(AttackSpec,
        #{attr:=#{hit:={single, Hit}, critical:={single, Crit}}},
        #{attr:=#{resist:={single, Res}, block:={single, Blo}, dodge:={single, Dod}}} )->

    % 获得攻击属性（魔法／物理），放招类型（普攻／技能），是否可以抵抗，
    % 是否护甲减免（不考虑），技能失败概率
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
    Result = element(Binned, {failed, MoveType, dodge, resist, block, critical}),
    Result.


% ========================= TRANSFER INSTRUCTIONS ==============================


trans({set, Imm, _, _}, Ref) ->
	ref:set(Ref, Imm);

%% 当Inc < 0，且作用的属性是HP时，即是造成伤害，进入伤害处理程序
trans({add, Damage, AtkSpec, Outcome}, {attr, hp, P}=ToWhom) when Damage < 0 ->

    {_, _, _, Absorbable, _} = AtkSpec,

    %% 处理护甲减免:1-护甲值*0.0001
    AbsorbedDamage = case Absorbable of
        absorbable ->
            ArmorRatio = 1 - ref:val({attr, armor, P}) * 0.0001,
            Damage * ArmorRatio;
        _ ->
            Damage
        end,

    %% 处理转盘结果
    CalculatedDamage = case Outcome of
        critical ->
            CritMult = ref:val({attr, critical_mult, P}),
            AbsorbedDamage * CritMult;
        attack ->
            AbsorbedDamage;
        cast ->
            Damage;
        resist ->
            AbsorbedDamage / 10 * rand:uniform();
        _ -> 0
    end,

    % 上述伤害值再乘以伤害倍数，即得到最终的伤害值
    FinalDamage = CalculatedDamage * ref:val({attr, damage_mult, P}),
    trans({set, ref:val(ToWhom) + FinalDamage, none, none}, ToWhom);

trans({add, Inc, _, _Outcome}, ToWhom) ->
    trans({set, ref:val(ToWhom) + Inc, none, none}, ToWhom);

trans({add_mul, Mul, AttackSpec, Outcome}, ToWhom) ->
    trans({add, ref:val(ToWhom) * Mul, AttackSpec, Outcome}, ToWhom);

trans({add_inc_mul, {Inc, Mul}, AttackSpec, Outcome}, ToWhom) ->
    trans({add, Inc * Mul, AttackSpec, Outcome}, ToWhom).


trans({{Opcode, Oper, AttackSpec}, {attr, Attr, P}}, O, D) ->

    % 获得双方的操作数
    RefOperand = case Oper of
        {Ref} -> ref:val(Ref, O, D);
        {Ref1, Ref2} -> {ref:val(Ref1, O, D), ref:val(Ref2, O, D)}
    end,

    % 得到转盘结果
    Outcome = roulette(AttackSpec, O, D),

    % 将转盘结果加入玩家context，并按结果计算伤害／技能效果，把结果保存在TransPsn里面
    Psn = ref:who(P, O, D),
    TheTransedOne = trans({Opcode, RefOperand, AttackSpec, Outcome}, {attr, Attr, Psn}),

    {TransO, TransD} = case P of
        off ->  {TheTransedOne, D};
        def ->  {O, TheTransedOne}
    end,

    {TransO, TransD}.



% ======================== APPLY ALL TRANSFERS IN A LIST ========================
% For each trans operation, apply_trans_with_log combines the player context with
% log. Since the transfers are written in a list, the apply_transes.g_nested will
% apply all the transfers sequentially over the player context, and returns log.

% Accepts cond description

get_player_log(P, Role, Order) ->
    #{player_name => maps:get(player_name, P),
      profession  => ref:val({attr, profession, P}),
      hp          => ref:val({attr, hp, P}),
      role        => Role,
      order       => Order
     }.

get_cast_effect_log(SkillName, CastOutcome) ->
    #{skill_name => SkillName,
      outcome    => CastOutcome,
      attr       => none,
      dest       =>none,
      diff       => 0
     }.

get_effect_log(SkillSpec, _, {O, D}) ->
    {SkillName, {_, {_, Attr, Who}}} = SkillSpec,

    Dest = case Who of
        off -> offender;
        _ -> defender
    end,

    #{skill_name => SkillName,
      outcome    => ref:val({attr, outcome, O}),
      attr       => Attr,
      dest       => Dest,
      diff       => ref:val({attr, diff, Who}, O, D)
     }.


log_cast(S, SkillName, IsSuccessful,
    #{player_id:=OID} = O,
    #{player_id:=DID} = D
) ->

    CastOutcome = case IsSuccessful of
        true -> successful;
        _ -> failed
    end,

    #{
        state  => maps:remove(offender, S),
        effect => get_cast_effect_log(SkillName, CastOutcome),
        OID    => get_player_log(O, offender, init),
        DID    => get_player_log(D, defender, init)
    }.

cast(S, #{casts:=Casts}=O, D, Log) ->
    cast(S, O, D, Log, Casts).

cast(S, O, D, Log, Casts) ->

    #{seq:=Seq} = S,
    #{state:=#{hp:={single, H1}}}=O,
    #{state:=#{hp:={single, H2}}}=D,
    #{attr:=#{cast_disabled:={single, CastDisabled}}}=O,

    if H1 =< 0 ->
           {O, D, Log};
       H2 =< 0 ->
           {O, D, Log};
       Casts == [] ->
           {O, D, Log};
       CastDisabled /= 0 ->
           {O, D, Log};
       true ->
           [{SeqIndex, SkillName, IsSuccessful} | Remaining] = Casts,
           
           NewLog = if Seq == SeqIndex ->
                           [log_cast(S, SkillName, IsSuccessful, O, D) | Log];
                       true -> Log
                    end,
           cast(S, O, D, NewLog, Remaining)
    end.

log_trans(#{stage:=Stage} = S, SkillSpec,
    #{player_id:=OID} = O,
    #{player_id:=DID} = D
) ->

    InitOrFollow = case Stage of
        casting -> init;
        _ -> follow
    end,

    #{
        state => maps:remove(offender, S),
        effect => get_effect_log(SkillSpec, none, {O, D}),
        OID => get_player_log(O, offender, InitOrFollow),
        DID => get_player_log(D, defender, InitOrFollow)
    }.


effect(S, #{effects:=Effects}=O, D, Log) ->
    effect(S, O, D, Log, Effects).

effect(_S, #{state:=#{hp:={single, H1}}}=O, #{state:=#{hp:={single, H2}}}=D, Log, _) when (H1 =< 0) or (H2 =< 0) ->
    {O, D, Log};

effect(_S, O, D, Log, []) ->
    {O, D, Log};

effect(S, O, D, Log, [ {_Index, Name, Conds, Trans, _Success} | Remaining]) ->

    {NewO, NewD, NewLog} = case conds:check(Conds, S, O, D) of
        true ->
            { TransedO, TransedD} = trans(Trans, O, D),
            TransLog = log_trans(S, {Name, Trans}, TransedO, TransedD),
            {TransedO, TransedD, [TransLog | Log]};
        _    ->
            {O, D, Log}
    end,

    effect(S, NewO, NewD, NewLog, Remaining).
