-module(blc_weapon).

-include("tacticians/attributes.hrl").
-include("tacticians/damage_types.hrl").

-define(WEAPON_ATTRIBUTE_RANGE,           rnge).
-define(WEAPON_ATTRIBUTE_RANGE_MIN,       0).
-define(WEAPON_ATTRIBUTE_RANGE_MAX,       2).
-define(WEAPON_ATTRIBUTE_RANGE_DEFAULT,   0).
-define(WEAPON_ATTRIBUTE_RANGE_COST,      200).

-define(WEAPON_RANGED_DEFENSE_RANGE, 1).

-define
(
   BASE_SPENDABLE_WEAPON_POINTS,
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

-define
(
   MELEE_SPENDABLE_WEAPON_POINTS,
   (
      ?BASE_SPENDABLE_WEAPON_POINTS
      +
      (
         ?ATTRIBUTE_PARRY_CHANCE_COST
         * (?ATTRIBUTE_PARRY_CHANCE_DEFAULT - ?ATTRIBUTE_PARRY_CHANCE_MIN)
      )
   )
).

-define(RANGED_SPENDABLE_WEAPON_POINTS, ?BASE_SPENDABLE_WEAPON_POINTS).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-type range_type() :: (melee | ranged).

-record
(
   proto_weapon,
   {
      range :: non_neg_integer(),
      range_type :: range_type(),
      omnimods :: shr_omnimods:type(),
      attack_coef :: list(blc_damage_type:coefficient()),
      attack_score :: non_neg_integer(),
      remaining_points :: non_neg_integer()
   }
).

-opaque type() :: #proto_weapon{}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export_type([type/0,range_type/0]).

-export
(
   [
      increase_range_by/2,
      increase_parry_chance_by/2,
      increase_accuracy_by/2,
      increase_critical_hit_chance_by/2,
      increase_double_hit_chance_by/2,
      increase_attack_score_by/2,
      increase_range_for/2,
      increase_parry_chance_for/2,
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
      new/2,
      get_remaining_points/1,
      generate/4,
      export/1,
      finalize/1
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec increase_attribute_by
   (
      (shr_attributes:enum() | ?WEAPON_ATTRIBUTE_RANGE),
      non_neg_integer(),
      type()
   )
   -> ({ok, type()} | blc_error:type()).
increase_attribute_by (?WEAPON_ATTRIBUTE_RANGE, S0Amount, Weapon) ->
   CurrentValue = Weapon#proto_weapon.range,

   S1Amount =
      case ((CurrentValue + S0Amount) > ?WEAPON_ATTRIBUTE_RANGE_MAX) of
         true -> (?WEAPON_ATTRIBUTE_RANGE_MAX - CurrentValue);
         false -> S0Amount
      end,

   Cost = (S1Amount * ?WEAPON_ATTRIBUTE_RANGE_COST),
   RemainingPoints = Weapon#proto_weapon.remaining_points,

   case (Cost > RemainingPoints) of
      true -> {error, balance, RemainingPoints, Cost};
      false ->
         {
            ok,
            Weapon#proto_weapon
            {
               remaining_points = (RemainingPoints - Cost),
               range = (CurrentValue + S1Amount)
            }
         }
   end;
increase_attribute_by (Attribute, S0Amount, Weapon) ->
   CurrentOmnimods = Weapon#proto_weapon.omnimods,
   CurrentValue =
      shr_omnimods:get_attribute_modifier(Attribute, CurrentOmnimods),

   {_AttMin, _AttDef, AttMax, AttCost} = blc_attribute:get_info(Attribute),

   S1Amount =
      case ((CurrentValue + S0Amount) > AttMax) of
         true -> (AttMax - CurrentValue);
         false -> S0Amount
      end,

   Cost = (S1Amount * AttCost),
   RemainingPoints = Weapon#proto_weapon.remaining_points,

   case (Cost > RemainingPoints) of
      true -> {error, balance, RemainingPoints, Cost};
      false ->
         {
            ok,
            Weapon#proto_weapon
            {
               remaining_points = (RemainingPoints - Cost),
               omnimods =
                  shr_omnimods:mod_attribute_modifier
                  (
                     Attribute,
                     S1Amount,
                     Weapon#proto_weapon.omnimods
                  )
            }
         }
   end.

-spec get_max_attribute_ratio
   (
      non_neg_integer(),
      shr_attributes:meta_enum()
      | ?WEAPON_ATTRIBUTE_RANGE
   )
   -> float().
get_max_attribute_ratio (SpendablePoints, ?WEAPON_ATTRIBUTE_RANGE) ->
   Contrib =
      (
         ?WEAPON_ATTRIBUTE_RANGE_COST
         * (?WEAPON_ATTRIBUTE_RANGE_MAX - ?WEAPON_ATTRIBUTE_RANGE_MIN)
      ),

   case (Contrib == 0) of
      true -> 0.0;
      false -> (SpendablePoints / Contrib) * 100.0
   end;
get_max_attribute_ratio (SpendablePoints, Attribute) ->
   {AttMin, _AttDef, AttMax, AttCost} = blc_attribute:get_info(Attribute),

   Contrib = (AttCost * (AttMax - AttMin)),

   case (Contrib == 0) of
      true -> 0.0;
      false -> (SpendablePoints / Contrib) * 100.0
   end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec increase_accuracy_by
   (
      non_neg_integer(),
      type()
   )
   -> ({ok, type()} | blc_error:type()).
increase_accuracy_by (Amount, Weapon) ->
   increase_attribute_by(?ATTRIBUTE_ACCURACY, Amount, Weapon).

-spec increase_range_by
   (
      non_neg_integer(),
      type()
   )
   -> ({ok, type()} | blc_error:type()).
increase_range_by (Amount, Weapon) ->
   increase_attribute_by(?WEAPON_ATTRIBUTE_RANGE, Amount, Weapon).

-spec increase_parry_chance_by
   (
      non_neg_integer(),
      type()
   )
   -> ({ok, type()} | blc_error:type()).
increase_parry_chance_by (Amount, Weapon) ->
   case (Weapon#proto_weapon.range_type) of
      ranged ->
         if
            (Amount == 0) -> {ok, Weapon};
            true -> {error, incompatible}
         end;

      _ -> increase_attribute_by(?ATTRIBUTE_PARRY_CHANCE, Amount, Weapon)
   end.

-spec increase_critical_hit_chance_by
   (
      non_neg_integer(),
      type()
   )
   -> ({ok, type()} | blc_error:type()).
increase_critical_hit_chance_by (Amount, Weapon) ->
   increase_attribute_by(?ATTRIBUTE_CRITICAL_HIT_CHANCE, Amount, Weapon).

-spec increase_double_hit_chance_by
   (
      non_neg_integer(),
      type()
   )
   -> ({ok, type()} | blc_error:type()).
increase_double_hit_chance_by (Amount, Weapon) ->
   increase_attribute_by(?ATTRIBUTE_DOUBLE_HIT_CHANCE, Amount, Weapon).

-spec increase_attack_score_by
   (
      non_neg_integer(),
      type()
   )
   -> ({ok, type()} | blc_error:type()).
increase_attack_score_by (S0Amount, Weapon) ->
   CurrentValue = Weapon#proto_weapon.attack_score,
   S0NewValue = CurrentValue + S0Amount,
   {S1Amount, S1NewValue} =
      case (S0NewValue > ?ATTRIBUTE_ATTACK_SCORE_MAX) of
         false -> {S0Amount, S0NewValue};
         true ->
            {
               (?ATTRIBUTE_ATTACK_SCORE_MAX - CurrentValue),
               ?ATTRIBUTE_ATTACK_SCORE_MAX
            }
      end,

   Cost = (S1Amount * ?ATTRIBUTE_ATTACK_SCORE_COST),
   RemainingPoints = Weapon#proto_weapon.remaining_points,

   case (Cost > RemainingPoints) of
      true -> {error, balance, RemainingPoints, Cost};
      false ->
         {
            ok,
            Weapon#proto_weapon
            {
               remaining_points = (RemainingPoints - Cost),
               attack_score = S1NewValue,
               omnimods =
                  shr_omnimods:set_attack_modifiers
                  (
                     blc_damage_type:generate_entries_from_score
                     (
                        S1NewValue,
                        Weapon#proto_weapon.attack_coef
                     ),
                     Weapon#proto_weapon.omnimods
                  )
            }
         }
   end.

-spec set_attack_coefficients
   (
      list(blc_damage_type:coefficient()),
      type()
   )
   -> type().
set_attack_coefficients (Coefficients, Weapon) ->
   NewCoefs = blc_damage_type:sort_entries(Coefficients),

   Weapon#proto_weapon
   {
      attack_coef = NewCoefs,
      omnimods =
         shr_omnimods:set_attack_modifiers
         (
            blc_damage_type:generate_entries_from_score
            (
               Weapon#proto_weapon.attack_score,
               NewCoefs
            ),
            Weapon#proto_weapon.omnimods
         )
   }.

-spec new (range_type(), list(blc_damage_type:coefficient())) -> type().
new (RangeType, Coefficients) ->
   SortedCoefficients = blc_damage_type:sort_entries(Coefficients),

   #proto_weapon
   {
      range = ?WEAPON_ATTRIBUTE_RANGE_MIN,
      range_type = RangeType,
      omnimods =
         shr_omnimods:new
         (
            [
               {?ATTRIBUTE_ACCURACY, ?ATTRIBUTE_ACCURACY_MIN},
               {?ATTRIBUTE_PARRY_CHANCE, ?ATTRIBUTE_PARRY_CHANCE_MIN},
               {
                  ?ATTRIBUTE_CRITICAL_HIT_CHANCE,
                  ?ATTRIBUTE_CRITICAL_HIT_CHANCE_MIN
               },
               {
                  ?ATTRIBUTE_DOUBLE_HIT_CHANCE,
                  ?ATTRIBUTE_DOUBLE_HIT_CHANCE_MIN
               }
            ],
            blc_damage_type:generate_entries_from_score
            (
               ?ATTRIBUTE_ATTACK_SCORE_MIN,
               SortedCoefficients
            ),
            []
         ),

      attack_coef = SortedCoefficients,
      attack_score = ?ATTRIBUTE_ATTACK_SCORE_MIN,
      remaining_points =
         case RangeType of
            melee -> ?MELEE_SPENDABLE_WEAPON_POINTS;
            ranged -> ?RANGED_SPENDABLE_WEAPON_POINTS
         end
   }.

-spec increase_range_for
   (
      non_neg_integer(),
      type()
   )
   -> ({ok, type()} | blc_error:type()).
increase_range_for (GivenPoints, Weapon) ->
   AmountOfIncrease = trunc(GivenPoints / ?WEAPON_ATTRIBUTE_RANGE_COST),
   increase_range_by(AmountOfIncrease, Weapon).

-spec increase_parry_chance_for
   (
      non_neg_integer(),
      type()
   )
   -> ({ok, type()} | blc_error:type()).
increase_parry_chance_for (GivenPoints, Weapon) ->
   AmountOfIncrease = trunc(GivenPoints / ?ATTRIBUTE_PARRY_CHANCE_COST),
   increase_parry_chance_by(AmountOfIncrease, Weapon).

-spec increase_accuracy_for
   (
      non_neg_integer(),
      type()
   )
   -> ({ok, type()} | blc_error:type()).
increase_accuracy_for (GivenPoints, Weapon) ->
   AmountOfIncrease = trunc(GivenPoints / ?ATTRIBUTE_ACCURACY_COST),
   increase_accuracy_by(AmountOfIncrease, Weapon).

-spec increase_critical_hit_chance_for
   (
      non_neg_integer(),
      type()
   )
   -> ({ok, type()} | blc_error:type()).
increase_critical_hit_chance_for (GivenPoints, Weapon) ->
   AmountOfIncrease = trunc(GivenPoints / ?ATTRIBUTE_CRITICAL_HIT_CHANCE_COST),
   increase_critical_hit_chance_by(AmountOfIncrease, Weapon).

-spec increase_double_hit_chance_for
   (
      non_neg_integer(),
      type()
   )
   -> ({ok, type()} | blc_error:type()).
increase_double_hit_chance_for (GivenPoints, Weapon) ->
   AmountOfIncrease = trunc(GivenPoints / ?ATTRIBUTE_DOUBLE_HIT_CHANCE_COST),
   increase_double_hit_chance_by(AmountOfIncrease, Weapon).


-spec increase_attack_score_for
   (
      non_neg_integer(),
      type()
   )
   -> ({ok, type()} | blc_error:type()).
increase_attack_score_for (GivenPoints, Weapon) ->
   AmountOfIncrease = trunc(GivenPoints / ?ATTRIBUTE_ATTACK_SCORE_COST),
   increase_attack_score_by(AmountOfIncrease, Weapon).


-spec get_remaining_points (type()) -> non_neg_integer().
get_remaining_points (Weapon) -> Weapon#proto_weapon.remaining_points.

-spec generate (range_type(), 0..100, 0..100, 0..100) -> list(type()).
generate (RangeType, AttributeMin, AttributeStep, ElementStep) ->
   BasePoints =
      case RangeType of
         ranged -> ?RANGED_SPENDABLE_WEAPON_POINTS;
         melee -> ?MELEE_SPENDABLE_WEAPON_POINTS
      end,

   MaxAccuracy = get_max_attribute_ratio(BasePoints, ?ATTRIBUTE_ACCURACY),
   MaxCriticalHitChance =
      get_max_attribute_ratio(BasePoints, ?ATTRIBUTE_CRITICAL_HIT_CHANCE),
   MaxDoubleHitChance =
      get_max_attribute_ratio(BasePoints, ?ATTRIBUTE_DOUBLE_HIT_CHANCE),
   MaxRange = get_max_attribute_ratio(BasePoints, ?WEAPON_ATTRIBUTE_RANGE),
   MaxParryChance =
      get_max_attribute_ratio(BasePoints, ?ATTRIBUTE_PARRY_CHANCE),
   MaxAttackScore =
      get_max_attribute_ratio(BasePoints, ?ATTRIBUTE_ATTACK_SCORE),

   AttributeCount =
      case RangeType of
         melee -> 6;
         _ -> 5
      end,

   Distributions =
      blc_distribution:generate(AttributeCount, AttributeMin, AttributeStep),

   ValidDistributions =
      lists:filtermap
      (
         fun (Input) ->
            [
               Accuracy,
               CriticalHitChance,
               DoubleHitChance,
               Range,
               AttackScore
               | MaybeParryChance
            ]
               = Input,
            ParryChance =
               case MaybeParryChance of
                  [] -> 0;
                  [E] -> E
               end,
            if
               (Accuracy > MaxAccuracy) -> false;
               (CriticalHitChance > MaxCriticalHitChance) -> false;
               (DoubleHitChance > MaxDoubleHitChance) -> false;
               (Range > MaxRange) -> false;
               (ParryChance > MaxParryChance) -> false;
               (AttackScore > MaxAttackScore) -> false;
               true ->
                  {
                     true,
                     [
                        {?ATTRIBUTE_ACCURACY, Accuracy},
                        {?ATTRIBUTE_CRITICAL_HIT_CHANCE, CriticalHitChance},
                        {?ATTRIBUTE_DOUBLE_HIT_CHANCE, DoubleHitChance},
                        {?WEAPON_ATTRIBUTE_RANGE, Range},
                        {?ATTRIBUTE_ATTACK_SCORE, AttackScore},
                        {?ATTRIBUTE_PARRY_CHANCE, ParryChance}%,
                     ]
                  }
            end
         end,
         Distributions
      ),

   BaseWeaponsCoefs = [{?DAMAGE_TYPE_SLASH, 100}],


   BaseWeapon = new(RangeType, BaseWeaponsCoefs),

   BaseWeapons =
      lists:map
      (
         fun (Distribution) ->
            PointsUsed =
               lists:map
               (
                  fun ({Attribute, Percent}) ->
                     {
                        Attribute,
                        trunc((BasePoints * Percent) / 100)
                     }
                  end,
                  Distribution
               ),

            FinalWeapon =
               lists:foldl
               (
                  fun ({Attribute, Points}, Weapon) ->
                     case Attribute of
                        ?ATTRIBUTE_ATTACK_SCORE ->
                           {ok, NewWeapon} =
                              increase_attack_score_for(Points, Weapon),
                           NewWeapon;

                        ?ATTRIBUTE_CRITICAL_HIT_CHANCE ->
                           {ok, NewWeapon} =
                              increase_critical_hit_chance_for(Points, Weapon),
                           NewWeapon;

                        ?WEAPON_ATTRIBUTE_RANGE ->
                           {ok, NewWeapon} =
                              increase_range_for(Points, Weapon),
                           NewWeapon;

                        ?ATTRIBUTE_ACCURACY ->
                           {ok, NewWeapon} =
                              increase_accuracy_for(Points, Weapon),
                           NewWeapon;

                        ?ATTRIBUTE_PARRY_CHANCE ->
                           {ok, NewWeapon} =
                              increase_parry_chance_for(Points, Weapon),
                           NewWeapon;

                        ?ATTRIBUTE_DOUBLE_HIT_CHANCE ->
                           {ok, NewWeapon} =
                              increase_double_hit_chance_for(Points, Weapon),
                           NewWeapon
                     end
                  end,
                  BaseWeapon,
                  PointsUsed
               ),

            lists:foldl
            (
               fun ({Attribute, _}, Weapon) ->
                  NewWeapon =
                     case Attribute of
                        ?ATTRIBUTE_ATTACK_SCORE ->
                           increase_attack_score_for
                           (
                              Weapon#proto_weapon.remaining_points,
                              Weapon
                           );

                        ?WEAPON_ATTRIBUTE_RANGE->
                           increase_range_for
                           (
                              Weapon#proto_weapon.remaining_points,
                              Weapon
                           );

                        _ -> increase_attribute_by(Attribute, 1, Weapon)
                     end,

                  case NewWeapon of
                     {ok, NextWeapon} -> NextWeapon;
                     _ -> Weapon
                  end
               end,
               FinalWeapon,
               lists:sort
               (
                  fun ({_AttributeA, ScoreA}, {_AttributeB, ScoreB}) ->
                     (ScoreA > ScoreB)
                  end,
                  lists:map
                  (
                     fun (Attribute) ->
                        case Attribute of
                           ?ATTRIBUTE_ATTACK_SCORE ->
                              {
                                 ?ATTRIBUTE_ATTACK_SCORE,
                                 FinalWeapon#proto_weapon.attack_score
                              };

                           _ ->
                              {
                                 Attribute,
                                 shr_omnimods:get_attribute_modifier
                                 (
                                    Attribute,
                                    FinalWeapon#proto_weapon.omnimods
                                 )
                              }
                        end
                     end,
                     [
                        ?ATTRIBUTE_ATTACK_SCORE,
                        ?ATTRIBUTE_CRITICAL_HIT_CHANCE,
                        ?WEAPON_ATTRIBUTE_RANGE,
                        ?ATTRIBUTE_ACCURACY,
                        ?ATTRIBUTE_DOUBLE_HIT_CHANCE
                     ]
                  )
               )
            )
         end,
         ValidDistributions
      ),

   shr_lists_util:product
   (
      fun (Weapon, ElementDistribution) ->
         set_attack_coefficients(ElementDistribution, Weapon)
      end,
      BaseWeapons,
      lists:map
      (
         fun ([A, B, C]) ->
            [
               {?DAMAGE_TYPE_SLASH, A},
               {?DAMAGE_TYPE_PIERCE, B},
               {?DAMAGE_TYPE_BLUNT, C}
            ]
         end,
         blc_distribution:generate(3, ElementStep)
      )
   ).

-spec export (type()) -> list().
export (Weapon) ->
   Range = Weapon#proto_weapon.range,
   {DefenseRange, AttackRange} =
      case Weapon#proto_weapon.range_type of
         melee -> {0, (Range + 1)};
         ranged ->
            {
               (?WEAPON_RANGED_DEFENSE_RANGE + Range),
               (?WEAPON_RANGED_DEFENSE_RANGE + Range + 2)
            }
      end,
   (
      io_lib:format("   ~B,~n   ~B,~n", [DefenseRange, AttackRange])
      ++ shr_omnimods:export(Weapon#proto_weapon.omnimods)
   ).

-spec finalize (type()) ->
   {
      range_type(),
      non_neg_integer(),
      shr_omnimods:type(),
      non_neg_integer()
   }.
finalize (Weapon) ->
   {
      Weapon#proto_weapon.range_type,
      Weapon#proto_weapon.range,
      Weapon#proto_weapon.omnimods,
      Weapon#proto_weapon.remaining_points
   }.
