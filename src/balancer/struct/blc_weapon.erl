-module(blc_weapon).

-include("tacticians/attributes.hrl").

-define(WEAPON_ATTRIBUTE_RANGE_MIN,       0).
-define(WEAPON_ATTRIBUTE_RANGE_MAX,       2).
-define(WEAPON_ATTRIBUTE_RANGE_DEFAULT,   0).
-define(WEAPON_ATTRIBUTE_RANGE_COST,      1).

-define(WEAPON_ATTRIBUTE_TYPE_MIN,     0).
-define(WEAPON_ATTRIBUTE_TYPE_MAX,     1).
-define(WEAPON_ATTRIBUTE_TYPE_DEFAULT, 0).
-define(WEAPON_ATTRIBUTE_TYPE_COST,    100).


-define
(
   SPENDABLE_WEAPON_POINTS,
   (
      (
         ?ATTRIBUTE_ACCURACY_COST
         * (?ATTRIBUTE_ACCURACY_DEFAULT - ?ATTRIBUTE_ACCURACY_MIN)
      )
      +
      (
         ?ATTRIBUTE_CRITICAL_HIT_CHANCE_COST
         *
         (
            ?ATTRIBUTE_CRITICAL_HIT_CHANCE_DEFAULT
            - ?ATTRIBUTE_CRITICAL_HIT_CHANCE_MIN
         )
      )
      +
      (
         ?ATTRIBUTE_DOUBLE_HIT_CHANCE_COST
         *
         (
            ?ATTRIBUTE_DOUBLE_HIT_CHANCE_DEFAULT
            - ?ATTRIBUTE_DOUBLE_HIT_CHANCE_MIN
         )
      )
      +
      (
         ?ATTRIBUTE_ATTACK_SCORE_COST
         * (?ATTRIBUTE_ATTACK_SCORE_DEFAULT - ?ATTRIBUTE_ATTACK_SCORE_MIN)
      )
      +
      (
         ?WEAPON_ATTRIBUTE_RANGE_COST
         * (?WEAPON_ATTRIBUTE_RANGE_DEFAULT - ?WEAPON_ATTRIBUTE_RANGE_MIN)
      )
   )
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record
(
   proto_weapon,
   {
      range :: non_neg_integer(),
      type :: non_neg_integer(),
      accuracy :: non_neg_integer(),
      critical_hit_chance :: non_neg_integer(),
      double_hit_chance :: non_neg_integer(),
      attack :: list(blc_damage_type:entry()),
      attack_coef :: list(blc_damage_type:coefficient()),
      attack_score :: non_neg_integer()
   }
).

-opaque proto_weapon() :: #proto_weapon{}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export_type([proto_weapon/0]).

-export
(
   [
      increase_range_by/2,
      increase_type_by/2,
      increase_accuracy_by/2,
      increase_critical_hit_chance_by/2,
      increase_double_hit_chance_by/2,
      increase_attack_score_by/2,
      increase_range_for/2,
      increase_type_for/2,
      increase_accuracy_for/2,
      increase_critical_hit_chance_for/2,
      increase_double_hit_chance_for/2,
      increase_attack_score_for/2,
      set_attack_coefficients/2
   ]
).

-export
(
   [
      new/1,
      get_spendable_points/0
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec increase_accuracy_by
   (
      non_neg_integer(),
      proto_weapon()
   )
   -> {proto_weapon(), non_neg_integer()}.
increase_accuracy_by (Amount, Weapon) ->
   NewAccuracy = (Weapon#proto_weapon.accuracy + Amount),
   case (NewAccuracy > ?ATTRIBUTE_ACCURACY_MAX) of
      true ->
         {
            Weapon#proto_weapon{ accuracy = ?ATTRIBUTE_ACCURACY_MAX },
            (
               (?ATTRIBUTE_ACCURACY_MAX - Weapon#proto_weapon.accuracy)
               * ?ATTRIBUTE_ACCURACY_COST
            )
         };

      false ->
         {
            Weapon#proto_weapon{ accuracy = NewAccuracy },
            (Amount * ?ATTRIBUTE_ACCURACY_COST)
         }
   end.

-spec increase_range_by
   (
      non_neg_integer(),
      proto_weapon()
   )
   -> {proto_weapon(), non_neg_integer()}.
increase_range_by (Amount, Weapon) ->
   NewDamageModifier = Weapon#proto_weapon.range + Amount,
   case (NewDamageModifier > ?WEAPON_ATTRIBUTE_RANGE_MAX) of
      true ->
         {
            Weapon#proto_weapon
            {
               range = ?WEAPON_ATTRIBUTE_RANGE_MAX
            },
            (
               (
                  ?WEAPON_ATTRIBUTE_RANGE_MAX
                  - Weapon#proto_weapon.range
               )
               * ?WEAPON_ATTRIBUTE_RANGE_COST
            )
         };

      false ->
         {
            Weapon#proto_weapon{ range = NewDamageModifier },
            (Amount * ?WEAPON_ATTRIBUTE_RANGE_COST)
         }
   end.

-spec increase_type_by
   (
      non_neg_integer(),
      proto_weapon()
   )
   -> {proto_weapon(), non_neg_integer()}.
increase_type_by (Amount, Weapon) ->
   NewType = Weapon#proto_weapon.type + Amount,
   case (NewType > ?WEAPON_ATTRIBUTE_TYPE_MAX) of
      true ->
         {
            Weapon#proto_weapon { type = ?WEAPON_ATTRIBUTE_TYPE_MAX },
            (
               (?WEAPON_ATTRIBUTE_TYPE_MAX - Weapon#proto_weapon.type)
               * ?WEAPON_ATTRIBUTE_TYPE_COST
            )
         };

      false ->
         {
            Weapon#proto_weapon{ type = NewType },
            (Amount * ?WEAPON_ATTRIBUTE_TYPE_COST)
         }
   end.

-spec increase_critical_hit_chance_by
   (
      non_neg_integer(),
      proto_weapon()
   )
   -> {proto_weapon(), non_neg_integer()}.
increase_critical_hit_chance_by (Amount, Weapon) ->
   NewCriticalHitChance = (Weapon#proto_weapon.critical_hit_chance + Amount),
   case (NewCriticalHitChance > ?ATTRIBUTE_CRITICAL_HIT_CHANCE_MAX) of
      true ->
         {
            Weapon#proto_weapon
            {
               critical_hit_chance = ?ATTRIBUTE_CRITICAL_HIT_CHANCE_MAX
            },
            (
               (
                  ?ATTRIBUTE_CRITICAL_HIT_CHANCE_MAX
                  - Weapon#proto_weapon.critical_hit_chance
               )
               * ?ATTRIBUTE_CRITICAL_HIT_CHANCE_COST
            )
         };

      false ->
         {
            Weapon#proto_weapon{ critical_hit_chance = NewCriticalHitChance },
            (Amount * ?ATTRIBUTE_CRITICAL_HIT_CHANCE_COST)
         }
   end.

-spec increase_double_hit_chance_by
   (
      non_neg_integer(),
      proto_weapon()
   )
   -> {proto_weapon(), non_neg_integer()}.
increase_double_hit_chance_by (Amount, Weapon) ->
   NewDoubleHitChance = Weapon#proto_weapon.double_hit_chance + Amount,
   case (NewDoubleHitChance > ?ATTRIBUTE_DOUBLE_HIT_CHANCE_MAX) of
      true ->
         {
            Weapon#proto_weapon
            {
               double_hit_chance = ?ATTRIBUTE_DOUBLE_HIT_CHANCE_MAX
            },
            (
               (
                  ?ATTRIBUTE_DOUBLE_HIT_CHANCE_MAX
                  - Weapon#proto_weapon.double_hit_chance
               )
               * ?ATTRIBUTE_DOUBLE_HIT_CHANCE_COST
            )
         };

      false ->
         {
            Weapon#proto_weapon{ double_hit_chance = NewDoubleHitChance },
            (Amount * ?ATTRIBUTE_DOUBLE_HIT_CHANCE_COST)
         }
   end.


-spec increase_attack_score_by
   (
      non_neg_integer(),
      proto_weapon()
   )
   -> {proto_weapon(), non_neg_integer()}.
increase_attack_score_by (Amount, Weapon) ->
   NewAttackScore = (Weapon#proto_weapon.attack_score + Amount),
   case (NewAttackScore > ?ATTRIBUTE_ATTACK_SCORE_MAX) of
      true ->
         {
            Weapon#proto_weapon
            {
               attack_score = ?ATTRIBUTE_ATTACK_SCORE_MAX,
               attack =
                  blc_damage_type:generate_entries_from_score
                  (
                     NewAttackScore,
                     Weapon#proto_weapon.attack_coef
                  )
            },
            (
               (?ATTRIBUTE_ATTACK_SCORE_MAX - Weapon#proto_weapon.attack_score)
               * Amount
            )
         };

      false ->
         {
            Weapon#proto_weapon
            {
               attack_score = NewAttackScore,
               attack =
                  blc_damage_type:generate_entries_from_score
                  (
                     NewAttackScore,
                     Weapon#proto_weapon.attack_coef
                  )
            },
            (Amount * ?ATTRIBUTE_ATTACK_SCORE_COST)
         }
   end.

-spec set_attack_coefficients
   (
      list(blc_damage_type:coefficient()),
      proto_weapon()
   )
   -> proto_weapon().
set_attack_coefficients (Coefficients, Weapon) ->
   {Result, 0} =
      increase_attack_score_by
      (
         0,
         Weapon#proto_weapon
         {
            attack_coef = blc_damage_type:sort_entries(Coefficients)
         }
      ),

   Result.

-spec new (list(blc_damage_type:coefficient())) -> proto_weapon().
new (Coefficients) ->
   {Result, _AttackScoreIncreaseCost} =
      increase_attack_score_by
      (
         ?ATTRIBUTE_ATTACK_SCORE_MIN,
         #proto_weapon
         {
            range = ?WEAPON_ATTRIBUTE_RANGE_MIN,
            type = ?WEAPON_ATTRIBUTE_TYPE_MIN,
            accuracy = ?ATTRIBUTE_ACCURACY_MIN,
            critical_hit_chance = ?ATTRIBUTE_CRITICAL_HIT_CHANCE_MIN,
            double_hit_chance = ?ATTRIBUTE_DOUBLE_HIT_CHANCE_MIN,
            attack = [],
            attack_coef = blc_damage_type:sort_entries(Coefficients),
            attack_score = 0
         }
      ),

   Result.

-spec increase_range_for
   (
      non_neg_integer(),
      proto_weapon()
   )
   -> {proto_weapon(), non_neg_integer()}.
increase_range_for (GivenPoints, Weapon) ->
   AmountOfIncrease = trunc(GivenPoints / ?WEAPON_ATTRIBUTE_RANGE_COST),
   {Result, SpentPoints} = increase_range_by(AmountOfIncrease, Weapon),
   {Result, (GivenPoints - SpentPoints)}.

-spec increase_type_for
   (
      non_neg_integer(),
      proto_weapon()
   )
   -> {proto_weapon(), non_neg_integer()}.
increase_type_for (GivenPoints, Weapon) ->
   AmountOfIncrease = trunc(GivenPoints / ?WEAPON_ATTRIBUTE_TYPE_COST),
   {Result, SpentPoints} = increase_type_by(AmountOfIncrease, Weapon),
   {Result, (GivenPoints - SpentPoints)}.

-spec increase_accuracy_for
   (
      non_neg_integer(),
      proto_weapon()
   )
   -> {proto_weapon(), non_neg_integer()}.
increase_accuracy_for (GivenPoints, Weapon) ->
   AmountOfIncrease = trunc(GivenPoints / ?ATTRIBUTE_ACCURACY_COST),
   {Result, SpentPoints} = increase_accuracy_by(AmountOfIncrease, Weapon),
   {Result, (GivenPoints - SpentPoints)}.

-spec increase_critical_hit_chance_for
   (
      non_neg_integer(),
      proto_weapon()
   )
   -> {proto_weapon(), non_neg_integer()}.
increase_critical_hit_chance_for (GivenPoints, Weapon) ->
   AmountOfIncrease = trunc(GivenPoints / ?ATTRIBUTE_CRITICAL_HIT_CHANCE_COST),
   {Result, SpentPoints} =
      increase_critical_hit_chance_by(AmountOfIncrease, Weapon),

   {Result, (GivenPoints - SpentPoints)}.

-spec increase_double_hit_chance_for
   (
      non_neg_integer(),
      proto_weapon()
   )
   -> {proto_weapon(), non_neg_integer()}.
increase_double_hit_chance_for (GivenPoints, Weapon) ->
   AmountOfIncrease = trunc(GivenPoints / ?ATTRIBUTE_DOUBLE_HIT_CHANCE_COST),
   {Result, SpentPoints} =
      increase_double_hit_chance_by(AmountOfIncrease, Weapon),

   {Result, (GivenPoints - SpentPoints)}.


-spec increase_attack_score_for
   (
      non_neg_integer(),
      proto_weapon()
   )
   -> {proto_weapon(), non_neg_integer()}.
increase_attack_score_for (GivenPoints, Weapon) ->
   AmountOfIncrease = trunc(GivenPoints / ?ATTRIBUTE_ATTACK_SCORE_COST),
   {Result, SpentPoints} = increase_attack_score_by(AmountOfIncrease, Weapon),
   {Result, (GivenPoints - SpentPoints)}.


-spec get_spendable_points () -> non_neg_integer().
get_spendable_points () -> ?SPENDABLE_WEAPON_POINTS.
