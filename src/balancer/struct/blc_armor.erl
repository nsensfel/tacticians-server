-module(blc_armor).

-include("tacticians/attributes.hrl").
-include("tacticians/damage_types.hrl").

-define
(
   SPENDABLE_ARMOR_POINTS,
   (
      (
         ?ATTRIBUTE_DAMAGE_MODIFIER_COST
         * (?ATTRIBUTE_DAMAGE_MODIFIER_DEFAULT - ?ATTRIBUTE_DAMAGE_MODIFIER_MIN)
      )
      +
      (
         ?ATTRIBUTE_DODGE_CHANCE_COST
         * (?ATTRIBUTE_DODGE_CHANCE_DEFAULT - ?ATTRIBUTE_DODGE_CHANCE_MIN)
      )
      +
      (
         ?ATTRIBUTE_MOVEMENT_POINTS_COST
         * (?ATTRIBUTE_MOVEMENT_POINTS_DEFAULT - ?ATTRIBUTE_MOVEMENT_POINTS_MIN)
      )
      +
      (
         ?ATTRIBUTE_HEALTH_COST
         * (?ATTRIBUTE_HEALTH_DEFAULT - ?ATTRIBUTE_HEALTH_MIN)
      )
      +
      (
         ?ATTRIBUTE_DEFENSE_SCORE_COST
         * (?ATTRIBUTE_DEFENSE_SCORE_DEFAULT - ?ATTRIBUTE_DEFENSE_SCORE_MIN)
      )
   )
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record
(
   proto_armor,
   {
      omnimods :: shr_omnimods:type(),
      defense_coef :: list(blc_damage_type:coefficient()),
      defense_score :: non_neg_integer(),
      remaining_points :: non_neg_integer()
   }
).

-opaque type() :: #proto_armor{}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export_type([type/0]).

-export
(
   [
      increase_health_by/2,
      increase_damage_modifier_by/2,
      increase_dodge_chance_by/2,
      increase_defense_score_by/2,
      increase_movement_points_by/2,
      increase_health_for/2,
      increase_damage_modifier_for/2,
      increase_dodge_chance_for/2,
      increase_movement_points_for/2,
      increase_defense_score_for/2,
      set_defense_coefficients/2
   ]
).

-export
(
   [
      new/1,
      get_remaining_points/1,
      generate/3,
      finalize/1,
      export/1
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec increase_attribute_by
   (
      shr_attributes:enum(),
      non_neg_integer(),
      type()
   )
   -> ({ok, type()} | blc_error:type()).
increase_attribute_by (Attribute, S0Amount, Armor) ->
   CurrentOmnimods = Armor#proto_armor.omnimods,
   CurrentValue =
      shr_omnimods:get_attribute_modifier(Attribute, CurrentOmnimods),

   {_AttMin, _AttDef, AttMax, AttCost} = blc_attribute:get_info(Attribute),

   S1Amount =
      case ((CurrentValue + S0Amount) > AttMax) of
         true -> (AttMax - CurrentValue);
         false -> S0Amount
      end,

   Cost = (S1Amount * AttCost),
   RemainingPoints = Armor#proto_armor.remaining_points,

   case (Cost > RemainingPoints) of
      true -> {error, balance, RemainingPoints, Cost};
      false ->
         {
            ok,
            Armor#proto_armor
            {
               remaining_points = (RemainingPoints - Cost),
               omnimods =
                  shr_omnimods:mod_attribute_modifier
                  (
                     Attribute,
                     S1Amount,
                     Armor#proto_armor.omnimods
                  )
            }
         }
   end.

-spec get_max_attribute_ratio (shr_attributes:meta_enum()) -> float().
get_max_attribute_ratio (Attribute) ->
   {AttMin, _AttDef, AttMax, AttCost} = blc_attribute:get_info(Attribute),

   Contrib = (AttCost * (AttMax - AttMin)),

   case (Contrib == 0) of
      true -> 0.0;
      false -> (?SPENDABLE_ARMOR_POINTS / Contrib) * 100.0
   end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec increase_health_by
   (
      non_neg_integer(),
      type()
   )
   -> ({ok, type()} | blc_error:type()).
increase_health_by (Amount, Armor) ->
   increase_attribute_by(?ATTRIBUTE_HEALTH, Amount, Armor).

-spec increase_damage_modifier_by
   (
      non_neg_integer(),
      type()
   )
   -> ({ok, type()} | blc_error:type()).
increase_damage_modifier_by (Amount, Armor) ->
   increase_attribute_by(?ATTRIBUTE_DAMAGE_MODIFIER, Amount, Armor).

-spec increase_dodge_chance_by
   (
      non_neg_integer(),
      type()
   )
   -> ({ok, type()} | blc_error:type()).
increase_dodge_chance_by (Amount, Armor) ->
   increase_attribute_by(?ATTRIBUTE_DODGE_CHANCE, Amount, Armor).

-spec increase_movement_points_by
   (
      non_neg_integer(),
      type()
   )
   -> ({ok, type()} | blc_error:type()).
increase_movement_points_by (Amount, Armor) ->
   increase_attribute_by(?ATTRIBUTE_MOVEMENT_POINTS, Amount, Armor).

-spec increase_defense_score_by
   (
      non_neg_integer(),
      type()
   )
   -> ({ok, type()} | blc_error:type()).
increase_defense_score_by (S0Amount, Armor) ->
   CurrentValue = Armor#proto_armor.defense_score,
   S0NewValue = CurrentValue + S0Amount,
   {S1Amount, S1NewValue} =
      case (S0NewValue > ?ATTRIBUTE_DEFENSE_SCORE_MAX) of
         false -> {S0Amount, S0NewValue};
         true ->
            {
               (?ATTRIBUTE_DEFENSE_SCORE_MAX - CurrentValue),
               ?ATTRIBUTE_DEFENSE_SCORE_MAX
            }
      end,

   Cost = (S1Amount * ?ATTRIBUTE_DEFENSE_SCORE_COST),
   RemainingPoints = Armor#proto_armor.remaining_points,

   case (Cost > RemainingPoints) of
      true -> {error, balance, RemainingPoints, Cost};
      false ->
         {
            ok,
            Armor#proto_armor
            {
               remaining_points = (RemainingPoints - Cost),
               defense_score = S1NewValue,
               omnimods =
                  shr_omnimods:set_defense_modifiers
                  (
                     blc_damage_type:generate_entries_from_score
                     (
                        S1NewValue,
                        Armor#proto_armor.defense_coef
                     ),
                     Armor#proto_armor.omnimods
                  )
            }
         }
   end.

-spec set_defense_coefficients
   (
      list(blc_damage_type:coefficient()),
      type()
   )
   -> type().
set_defense_coefficients (Coefficients, Armor) ->
   NewCoefs = blc_damage_type:sort_entries(Coefficients),

   Armor#proto_armor
   {
      defense_coef = NewCoefs,
      omnimods =
         shr_omnimods:set_defense_modifiers
         (
            blc_damage_type:generate_entries_from_score
            (
               Armor#proto_armor.defense_score,
               NewCoefs
            ),
            Armor#proto_armor.omnimods
         )
   }.


-spec new (list(blc_damage_type:coefficient())) -> type().
new (Coefficients) ->
   SortedCoefficients = blc_damage_type:sort_entries(Coefficients),

   #proto_armor
   {
      omnimods =
         shr_omnimods:new
         (
            [
               {?ATTRIBUTE_HEALTH, ?ATTRIBUTE_HEALTH_MIN},
               {?ATTRIBUTE_DAMAGE_MODIFIER, ?ATTRIBUTE_DAMAGE_MODIFIER_MIN},
               {?ATTRIBUTE_DODGE_CHANCE, ?ATTRIBUTE_DODGE_CHANCE_MIN},
               {?ATTRIBUTE_MOVEMENT_POINTS, ?ATTRIBUTE_MOVEMENT_POINTS_MIN}
            ],
            [],
            blc_damage_type:generate_entries_from_score
            (
               ?ATTRIBUTE_DEFENSE_SCORE_MIN,
               SortedCoefficients
            )
         ),

      defense_coef = SortedCoefficients,
      defense_score = ?ATTRIBUTE_DEFENSE_SCORE_MIN,
      remaining_points = ?SPENDABLE_ARMOR_POINTS
   }.

-spec increase_health_for
   (
      non_neg_integer(),
      type()
   )
   -> ({ok, type()} | blc_error:type()).
increase_health_for (GivenPoints, Armor) ->
   AmountOfIncrease = trunc(GivenPoints / ?ATTRIBUTE_HEALTH_COST),
   increase_health_by(AmountOfIncrease, Armor).

-spec increase_damage_modifier_for
   (
      non_neg_integer(),
      type()
   )
   -> ({ok, type()} | blc_error:type()).
increase_damage_modifier_for (GivenPoints, Armor) ->
   AmountOfIncrease = trunc(GivenPoints / ?ATTRIBUTE_DAMAGE_MODIFIER_COST),
   increase_damage_modifier_by(AmountOfIncrease, Armor).

-spec increase_dodge_chance_for
   (
      non_neg_integer(),
      type()
   )
   -> ({ok, type()} | blc_error:type()).
increase_dodge_chance_for (GivenPoints, Armor) ->
   AmountOfIncrease = trunc(GivenPoints / ?ATTRIBUTE_DODGE_CHANCE_COST),
   increase_dodge_chance_by(AmountOfIncrease, Armor).

-spec increase_movement_points_for
   (
      non_neg_integer(),
      type()
   )
   -> ({ok, type()} | blc_error:type()).
increase_movement_points_for (GivenPoints, Armor) ->
   AmountOfIncrease = trunc(GivenPoints / ?ATTRIBUTE_MOVEMENT_POINTS_COST),
   increase_movement_points_by(AmountOfIncrease, Armor).

-spec increase_defense_score_for
   (
      non_neg_integer(),
      type()
   )
   -> ({ok, type()} | blc_error:type()).
increase_defense_score_for (GivenPoints, Armor) ->
   AmountOfIncrease = trunc(GivenPoints / ?ATTRIBUTE_DEFENSE_SCORE_COST),
   increase_defense_score_by(AmountOfIncrease, Armor).

-spec get_remaining_points (type()) -> non_neg_integer().
get_remaining_points (Armor) -> Armor#proto_armor.remaining_points.

-spec generate (0..100, 0..100, 0..100) -> list(type()).
generate (AttributeMin, AttributeStep, ElementStep) ->
   MaxDamageModifier = get_max_attribute_ratio(?ATTRIBUTE_DAMAGE_MODIFIER),
   MaxDodgeChance = get_max_attribute_ratio(?ATTRIBUTE_DODGE_CHANCE),
   MaxMovementPoints = get_max_attribute_ratio(?ATTRIBUTE_MOVEMENT_POINTS),
   MaxHealth = get_max_attribute_ratio(?ATTRIBUTE_HEALTH),
   MaxDefenseScore = get_max_attribute_ratio(?ATTRIBUTE_DEFENSE_SCORE),

   Distributions = blc_distribution:generate(5, AttributeMin, AttributeStep),
   ValidDistributions =
      lists:filtermap
      (
         fun
         (
            [
               DamageModifier,
               DodgeChance,
               MovementPoints,
               Health,
               DefenseScore
            ]
         ) ->
            if
               (DamageModifier > MaxDamageModifier) -> false;
               (DodgeChance > MaxDodgeChance) -> false;
               (MovementPoints > MaxMovementPoints) -> false;
               (Health > MaxHealth) -> false;
               (DefenseScore > MaxDefenseScore) -> false;
               true ->
                  {
                     true,
                     [
                        {?ATTRIBUTE_DAMAGE_MODIFIER, DamageModifier},
                        {?ATTRIBUTE_DODGE_CHANCE, DodgeChance},
                        {?ATTRIBUTE_MOVEMENT_POINTS, MovementPoints},
                        {?ATTRIBUTE_HEALTH, Health},
                        {?ATTRIBUTE_DEFENSE_SCORE, DefenseScore}%,
%                        {
%                           extra,
%                           (
%                              100
%                              -
%                              (
%                                 DamageModifier
%                                 + DodgeChance
%                                 + MovementPoints
%                                 + Health
%                                 + DefenseScore
%                              )
%                           )
%                        }
                     ]
                  }
            end
         end,
         Distributions
      ),

   BaseArmorsCoefs = [{?DAMAGE_TYPE_SLASH, 100}],

   BaseArmor = new(BaseArmorsCoefs),
   BaseArmors =
      lists:map
      (
         fun (Distribution) ->
            PointsUsed =
               lists:map
               (
                  fun ({Attribute, Percent}) ->
                     {
                        Attribute,
                        trunc((?SPENDABLE_ARMOR_POINTS * Percent) / 100)
                     }
                  end,
                  Distribution
               ),

            FinalArmor =
               lists:foldl
               (
                  fun ({Attribute, Points}, Armor) ->
                     case Attribute of
                        ?ATTRIBUTE_DEFENSE_SCORE ->
                           {ok, NewArmor} =
                              increase_defense_score_for(Points, Armor),
                           NewArmor;

                        ?ATTRIBUTE_DODGE_CHANCE ->
                           {ok, NewArmor} =
                              increase_dodge_chance_for(Points, Armor),
                           NewArmor;

                        ?ATTRIBUTE_HEALTH ->
                           {ok, NewArmor} =
                              increase_health_for(Points, Armor),
                           NewArmor;

                        ?ATTRIBUTE_DAMAGE_MODIFIER ->
                           {ok, NewArmor} =
                              increase_damage_modifier_for(Points, Armor),
                           NewArmor;

                        ?ATTRIBUTE_MOVEMENT_POINTS ->
                           {ok, NewArmor} =
                              increase_movement_points_for(Points, Armor),
                           NewArmor
                     end
                  end,
                  BaseArmor,
                  PointsUsed
               ),

            lists:foldl
            (
               fun ({Attribute, _}, Armor) ->
                  NewArmor =
                     case Attribute of
                        ?ATTRIBUTE_DEFENSE_SCORE ->
                           increase_defense_score_for
                           (
                              Armor#proto_armor.remaining_points,
                              Armor
                           );

                        _ -> increase_attribute_by(Attribute, 1, Armor)
                     end,

                  case NewArmor of
                     {ok, NextArmor} -> NextArmor;
                     _ -> Armor
                  end
               end,
               FinalArmor,
               lists:sort
               (
                  fun ({_AttributeA, ScoreA}, {_AttributeB, ScoreB}) ->
                     (ScoreA > ScoreB)
                  end,
                  lists:map
                  (
                     fun (Attribute) ->
                        case Attribute of
                           ?ATTRIBUTE_DEFENSE_SCORE ->
                              {
                                 ?ATTRIBUTE_DEFENSE_SCORE,
                                 FinalArmor#proto_armor.defense_score
                              };

                           _ ->
                              {
                                 Attribute,
                                 shr_omnimods:get_attribute_modifier
                                 (
                                    Attribute,
                                    FinalArmor#proto_armor.omnimods
                                 )
                              }
                        end
                     end,
                     [
                        ?ATTRIBUTE_DEFENSE_SCORE,
                        ?ATTRIBUTE_DODGE_CHANCE,
                        ?ATTRIBUTE_HEALTH,
                        ?ATTRIBUTE_DAMAGE_MODIFIER,
                        ?ATTRIBUTE_MOVEMENT_POINTS
                     ]
                  )
               )
            )
         end,
         ValidDistributions
      ),

   shr_lists_util:product
   (
      fun (Armor, ElementDistribution) ->
         set_defense_coefficients(ElementDistribution, Armor)
      end,
      BaseArmors,
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
export (Armor) -> shr_omnimods:export(Armor#proto_armor.omnimods).

-spec finalize (type()) -> {shr_omnimods:type(), non_neg_integer()}.
finalize (Armor) ->
   {
      Armor#proto_armor.omnimods,
      Armor#proto_armor.remaining_points
   }.
