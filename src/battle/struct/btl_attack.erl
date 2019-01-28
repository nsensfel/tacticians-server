-module(btl_attack).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-type order() :: ('first' | 'second' | 'counter').
-type precision() :: ('misses' | 'grazes' | 'hits').

-record
(
   attack,
   {
      order :: order(),
      precision :: precision(),
      is_critical :: boolean(),
      is_parry :: boolean(),
      damage :: non_neg_integer()
   }
).

-opaque type() :: #attack{}.
-type maybe_type() :: ('nothing' | type()).
-opaque step() :: {order(), boolean()}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export_type([type/0, maybe_type/0, step/0]).

-export
(
   [
      get_sequence/3,
      get_description_of/5,
      apply_to_healths/3
   ]
).

-export
(
   [
      encode/1
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec roll_precision
   (
      shr_statistics:type(),
      shr_statistics:type(),
      integer(),
      integer()
   )
   -> {precision(), integer(), integer()}.
roll_precision
(
   AttackerStatistics,
   DefenderStatistics,
   AttackerLuck,
   DefenderLuck
) ->
   DefenderDodges = shr_statistics:get_dodges(DefenderStatistics),
   AttackerAccuracy = shr_statistics:get_accuracy(AttackerStatistics),
   MissChance = max(0, (DefenderDodges - AttackerAccuracy)),

   {Roll, _IsSuccess, NewDefenderLuck, NewAttackerLuck} =
      shr_roll:conflict_with_luck
      (
         MissChance,
         DefenderLuck,
         AttackerLuck
      ),

   {
      case Roll of
         X when (X =< MissChance) -> misses;
         X when (X =< (MissChance * 2)) -> grazes;
         _ -> hits
      end,
      NewAttackerLuck,
      NewDefenderLuck
   }.

-spec roll_critical_hit
   (
      shr_statistics:type(),
      integer()
   )
   -> {boolean(), integer()}.
roll_critical_hit (AttackerStatistics, AttackerLuck) ->
   CriticalHitChance = shr_statistics:get_critical_hits(AttackerStatistics),
   {_Roll, IsSuccess, NewLuck} =
      shr_roll:percentage_with_luck
      (
         CriticalHitChance,
         AttackerLuck
      ),

   {IsSuccess, NewLuck}.

-spec roll_parry
   (
      shr_statistics:type(),
      integer()
   )
   -> {boolean(), integer()}.
roll_parry (DefenderStatistics, DefenderLuck) ->
   DefenderParryChance = shr_statistics:get_parries(DefenderStatistics),
   {_Roll, IsSuccess, NewLuck} =
      shr_roll:percentage_with_luck
      (
         DefenderParryChance,
         DefenderLuck
      ),

   {IsSuccess, NewLuck}.

-spec get_damage
   (
      precision(),
      boolean(),
      float(),
      shr_omnimods:type(),
      shr_omnimods:type()
   )
   -> non_neg_integer().
get_damage (Precision, IsCritical, AtkModifier, ActualAtkOmni, ActualDefOmni) ->
   S0DamageMultiplier =
      case Precision of
         misses -> 0;
         grazes -> 0.5;
         hits -> 1
      end,

   S1DamageMultiplier =
      case IsCritical of
         true -> (S0DamageMultiplier * 2);
         _ -> S0DamageMultiplier
      end,

   S2DamageMultiplier = (S1DamageMultiplier * AtkModifier),

   ActualDamage =
      shr_omnimods:get_attack_damage
      (
         S2DamageMultiplier,
         ActualAtkOmni,
         ActualDefOmni
      ),

   ActualDamage.

-spec effect_of_attack
   (
      order(),
      btl_character_current_data:type(),
      btl_character_current_data:type(),
      boolean(),
      integer(),
      integer()
   )
   -> {type(), integer(), integer()}.
effect_of_attack
(
   Order,
   AtkCurrData,
   DefCurrData,
   CanParry,
   AttackerLuck,
   DefenderLuck
) ->
   DefStats = btl_character_current_data:get_statistics(DefCurrData),

   {ParryIsSuccessful, S0DefLuck} =
      case CanParry of
         true -> roll_parry(DefStats, DefenderLuck);
         false -> {false, DefenderLuck}
      end,

   {
      ActualAtkData,
      ActualDefData,
      ActualAtkLuck,
      ActualDefLuck
   } =
      case ParryIsSuccessful of
         true -> {DefCurrData, AtkCurrData, S0DefLuck, AttackerLuck};
         false -> {AtkCurrData, DefCurrData, AttackerLuck, S0DefLuck}
      end,

   ActualAtkStats = btl_character_current_data:get_statistics(ActualAtkData),
   ActualAtkOmni = btl_character_current_data:get_omnimods(ActualAtkData),
   ActualDefStats = btl_character_current_data:get_statistics(ActualDefData),
   ActualDefOmni = btl_character_current_data:get_omnimods(ActualDefData),

   {Precision, S0ActualAtkLuck, S0ActualDefLuck} =
      roll_precision
      (
         ActualAtkStats,
         ActualDefStats,
         ActualAtkLuck,
         ActualDefLuck
      ),

   {IsCritical, S1ActualAtkLuck} =
      roll_critical_hit(ActualAtkStats, S0ActualAtkLuck),

   AtkDamageModifier = shr_statistics:get_damage_modifier(ActualAtkStats),
   Damage =
      get_damage
      (
         Precision,
         IsCritical,
         AtkDamageModifier,
         ActualAtkOmni,
         ActualDefOmni
      ),

   {FinalAttackerLuck, FinalDefenderLuck} =
      case ParryIsSuccessful of
         true -> {S0ActualDefLuck, S1ActualAtkLuck};
         false -> {S1ActualAtkLuck, S0ActualDefLuck}
      end,

   {
      #attack
      {
         order = Order,
         precision = Precision,
         is_critical = IsCritical,
         is_parry = ParryIsSuccessful,
         damage = Damage
      },
      FinalAttackerLuck,
      FinalDefenderLuck
   }.

-spec encode_order (order()) -> binary().
encode_order (first) -> <<"f">>;
encode_order (counter) -> <<"c">>;
encode_order (second) -> <<"s">>.

-spec encode_precision (precision()) -> binary().
encode_precision (hits) -> <<"h">>;
encode_precision (grazes) -> <<"g">>;
encode_precision (misses) -> <<"m">>.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec get_description_of
   (
      step(),
      btl_character_current_data:type(),
      btl_character_current_data:type(),
      integer(),
      integer()
   )
   -> {maybe_type(), integer(), integer()}.
get_description_of
(
   {first, CanParry},
   AtkCurrData,
   DefCurrData,
   AtkLuck,
   DefLuck
) ->
   effect_of_attack
   (
      first,
      AtkCurrData,
      DefCurrData,
      CanParry,
      AtkLuck,
      DefLuck
   );
get_description_of
(
   {second, CanParry},
   AtkCurrData,
   DefCurrData,
   AtkLuck,
   DefLuck
) ->
   AtkStats = btl_character_current_data:get_statistics(AtkCurrData),
   AttackerDoubleAttackChance =
      shr_statistics:get_double_hits(AtkStats),
   {_Roll, IsSuccessful, NewAtkLuck} =
      shr_roll:percentage_with_luck(AttackerDoubleAttackChance, AtkLuck),

   case IsSuccessful of
      true ->
         effect_of_attack
         (
            second,
            AtkCurrData,
            DefCurrData,
            CanParry,
            NewAtkLuck,
            DefLuck
         );

      _ -> {nothing, NewAtkLuck, DefLuck}
   end;
get_description_of
(
   {counter, CanParry},
   AtkCurrData,
   DefCurrData,
   AtkLuck,
   DefLuck
) ->
   {Effect, NewDefLuck, NewAtkLuck} =
      effect_of_attack
      (
         counter,
         DefCurrData,
         AtkCurrData,
         CanParry,
         DefLuck,
         AtkLuck
      ),
   {Effect, NewAtkLuck, NewDefLuck}.


-spec apply_to_healths
   (
      maybe_type(),
      non_neg_integer(),
      non_neg_integer()
   )
   -> {maybe_type(), non_neg_integer(), non_neg_integer()}.
apply_to_healths
(
   nothing,
   AttackerHealth,
   DefenderHealth
) ->
   {nothing, AttackerHealth, DefenderHealth};
apply_to_healths
(
   _Attack,
   AttackerHealth,
   DefenderHealth
)
when
(
   (AttackerHealth =< 0)
   or (DefenderHealth =< 0)
) ->
   {nothing, AttackerHealth, DefenderHealth};
apply_to_healths
(
   Attack,
   AttackerHealth,
   DefenderHealth
)
when
(
   (
      (not Attack#attack.is_parry)
      and ((Attack#attack.order == first) or (Attack#attack.order == second))
   )
   or
   (
      Attack#attack.is_parry
      and (Attack#attack.order == counter)
   )
) ->
   Damage = Attack#attack.damage,

   {
      Attack,
      AttackerHealth,
      (DefenderHealth - Damage)
   };
apply_to_healths
(
   Attack,
   AttackerHealth,
   DefenderHealth
)
when
(
   (
      (not Attack#attack.is_parry)
      and (Attack#attack.order == counter)
   )
   or
   (
      Attack#attack.is_parry
      and ((Attack#attack.order == first) or (Attack#attack.order == second))
   )
) ->
   Damage = Attack#attack.damage,

   {
      Attack,
      (AttackerHealth - Damage),
      DefenderHealth
   }.

-spec get_sequence
   (
      non_neg_integer(),
      shr_weapon:type(),
      shr_weapon:type()
   )
   -> list(step()).
get_sequence (AttackRange, AttackerWeapon, DefenderWeapon) ->
   AttackerDefenseRange = shr_weapon:get_minimum_range(AttackerWeapon),
   AttackerAttackRange =  shr_weapon:get_maximum_range(AttackerWeapon),
   DefenderDefenseRange = shr_weapon:get_minimum_range(DefenderWeapon),
   DefenderAttackRange =  shr_weapon:get_maximum_range(DefenderWeapon),

   AttackerCanAttack = (AttackRange =< AttackerAttackRange),
   AttackerCanDefend =
      (AttackerCanAttack and (AttackRange > AttackerDefenseRange)),

   true = (AttackerCanAttack == true),

   DefenderCanAttack = (AttackRange =< DefenderAttackRange),
   DefenderCanDefend =
      (DefenderCanAttack and (AttackRange > DefenderDefenseRange)),

   First = {first, DefenderCanDefend},
   Second = {second, DefenderCanDefend},
   Counter = {counter, AttackerCanDefend},

   case DefenderCanDefend of
      true -> [First, Counter, Second];
      _ -> [First, Second]
   end.

-spec encode (type()) -> {list(any())}.
encode (Attack) ->
   Order = Attack#attack.order,
   Precision = Attack#attack.precision,
   IsCritical = Attack#attack.is_critical,
   IsParry = Attack#attack.is_parry,
   Damage = Attack#attack.damage,

   {
      [
         {<<"ord">>, encode_order(Order)},
         {<<"pre">>, encode_precision(Precision)},
         {<<"cri">>, IsCritical},
         {<<"par">>, IsParry},
         {<<"dmg">>, Damage}
      ]
   }.
