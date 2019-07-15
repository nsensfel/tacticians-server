-module(blc_armor).

-include('base_attributes.hrl').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-type defense_entry() :: {atom(), non_neg_integer()}.

-record
(
   proto_armor,
   {
      health :: non_neg_integer(),
      damage_modifier :: non_neg_integer(),
      dodge :: non_neg_integer(),
      defense :: list(defense_entry()),
      defense_score :: non_neg_integer()
   }
).

-record
(
   factors,
   {
      health :: float(),
      damage_modifier :: float(),
      dodge :: float(),
      defense :: float()
   }
).

-opaque proto_armor() :: #proto_armor{}.
-opaque factors() :: #factors{}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export_type([proto_armor/0, armor/0, factors/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% Generic %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec get_relative_attribute
   (
      non_neg_integer(),
      non_neg_integer(),
      non_neg_integer()
   )
   -> non_neg_integer().
get_relative_attribute (Value, BaseValue, BaseTarget) ->
   (
      BaseTarget
      -
      math:floor((Value - BaseValue) * (BaseTarget/BaseValue))
   ).

%%%% Defense %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec calc_defense_score (list(defense_entry())) -> non_neg_integer().
calc_defense_score (Defense) ->
   DescSortedDefense =
      lists:sort
      (
         fun ({_NameA, ValueA}, {_NameB, ValueB}) ->
            (ValueA >= ValueB)
         end,
         Defense
      ),

   {_LastIndex, Result} =
      lists:foldl
      (
         fun (E, {Index, Current}) ->
            {(Index + 1), (Current + (Index * E))}
         end,
         {1, 0},
         DescSortedDefense
      ),

   Result.

-spec apply_defense_score_modifier
   (
      non_neg_integer(),
      integer(),
      list(defense_entry())
   )
   -> list(defense_entry()).
apply_defense_score_modifier (AbsModifier, Mod, S0DescSortedDefense) ->
   {S1DescSortedDefense, EndModifier} =
      lists:mapfoldl
      (
         fun ({Name, S0Value}, {Index, RemainingModifier}) ->
            case ((RemainingModifier > 0) and (S0Value > 0)) of
               true ->
                  S1Value = (S0Value + Mod),
                  {
                     {Name, S1Value},
                     {
                        (Index + 1),
                        RemainingModifier - (Index * S1Value)
                     }
                  };

               false -> {{Name, S0Value}, {(Index + 1), RemainingModifier}}
            end
         end,
         {1, AbsModifier},
         S0DescSortedDefense
      ),

   case (EndModifier > 0) of
      false -> S1DescSortedDefense;
      true ->
         apply_defense_score_modifier
         (
            EndModifier,
            Mod,
            S1DescSortedDefense
         )
   end.

%%%% Movement Points %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec get_maximum_movement_points_with_factors
   (
      non_neg_integer(),
      non_neg_integer(),
      float()
   )
   -> non_neg_integer().
get_maximum_movement_points_with_factors (Value, BaseValue, Factor) ->
   math:floor((Value * ?BASE_MOVEMENT_POINTS_ATTRIBUTE) / (BaseValue * Factor)).

-spec get_maximum_factor_with_movement_points
   (
      non_neg_integer(),
      non_neg_integer(),
      non_neg_integer()
   )
   -> float().
get_maximum_factor_with_movement_points (Value, BaseValue, MovementPoints) ->
   (
      Value
      /
      (
         (
            (MovementPoints / ?BASE_MOVEMENT_POINTS_ATTRIBUTE)
            + 1
         )
         * BaseValue
      )
   ).

-spec proto_armor_auto_dodge
   (
      list(defense_entry()),
      non_neg_integer(),
      non_neg_integer()
   )
   -> proto_armor().
proto_armor_auto_dodge (Defense, Health, DamageModifier) ->
   DefenseScore = get_defense_score(Defense),

   Dodge =
      relative_attribute
      (
         DefenseScore,
         ?BASE_DEFENSE_SCORE,
         ?BASE_DODGE_ATTRIBUTE
      ),

   #proto_armor
   {
      health = Health,
      damage_modifier = DamageModifier,
      dodge = Dodge,
      defense = Defense,
      defense_score = DefenseScore
   }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec get_maximum_damage_modifier () -> non_neg_integer().
get_maximum_damage_modifier () -> (?BASE_DAMAGE_MODIFIER_ATTRIBUTE * 2).

-spec get_maximum_movement_points () -> non_neg_integer().
get_maximum_movement_points () -> (?BASE_MOVEMENT_POINTS_ATTRIBUTE * 2).

-spec get_maximum_health () -> non_neg_integer().
get_maximum_health () -> (?BASE_HEALTH_ATTRIBUTE * 2).

-spec get_maximum_dodge () -> non_neg_integer().
get_maximum_dodge () -> (?BASE_DODGE_ATTRIBUTE * 2).

-spec get_maximum_defense_score () -> non_neg_integer().
get_maximum_defense_score () -> (?BASE_DEFENSE_SCORE * 2).


-spec get_maximum_movement_points
   (
      factors(),
      proto_armor()
   )
   -> non_neg_integer().
get_maximum_movement_points (Factor, ProtoArmor) ->
   lists:min
   (
      [
         get_maximum_movement_points_with_factors
         (
            ProtoArmor#proto_armor.health,
            ?BASE_HEALTH_ATTRIBUTE,
            Factor#factors.health
         ),
         get_maximum_movement_points_with_factors
         (
            ProtoArmor#proto_armor.damage_modifier,
            ?BASE_DAMAGE_MODIFIER_ATTRIBUTE,
            Factor#factors.damage_modifier
         ),
         get_maximum_movement_points_with_factors
         (
            ProtoArmor#proto_armor.defense_score,
            ?BASE_DEFENSE_SCORE,
            Factor#factors.defense
         ),
         get_maximum_movement_points_with_factors
         (
            ProtoArmor#proto_armor.dodge,
            ?BASE_DODGE_ATTRIBUTE,
            Factor#factors.dodge
         )
      ]
   ).

-spec get_maximum_factorss (non_neg_integer(), proto_armor()) -> factors().
get_maximum_factorss (MovementPoints, ProtoArmor) ->
   #proto_armor
   {
      health =
         get_maximum_factor_with_movement_points
         (
            ProtoArmor#proto_armor.health,
            ?BASE_HEALTH_ATTRIBUTE,
            MovementPoints
         ),
      damage_modifier =
         get_maximum_factor_with_movement_points
         (
            ProtoArmor#proto_armor.damage_modifier,
            ?BASE_DAMAGE_MODIFIER_ATTRIBUTE,
            MovementPoints
         ),
      dodge =
         get_maximum_factor_with_movement_points
         (
            ProtoArmor#proto_armor.dodge,
            ?BASE_DODGE_ATTRIBUTE,
            MovementPoints
         ),
      defense =
         get_maximum_factor_with_movement_points
         (
            ProtoArmor#proto_armor.defense_score,
            ?BASE_DEFENSE_SCORE,
            MovementPoints
         )
   }.

-spec proto_armor_through_health
   (
      list(defense_entry()),
      non_neg_integer()
   )
   -> proto_armor().
proto_armor_through_health (Defense, Health) ->
   DamageModifier =
      relative_attribute
      (
         Health,
         ?BASE_HEALTH_ATTRIBUTE,
         ?BASE_DAMAGE_MODIFIER_ATTRIBUTE
      ),

   proto_armor_auto_dodge(Defense, Health, DamageModifier).

-spec proto_armor_through_damage_modifier
   (
      list(defense_entry()),
      non_neg_integer()
   )
   -> proto_armor().
proto_armor_through_damage_modifier (Defense, DamageModifier) ->
   Health =
      relative_attribute
      (
         DamageModifier,
         ?BASE_DAMAGE_MODIFIER_ATTRIBUTE,
         ?BASE_HEALTH_ATTRIBUTE
      ),

   proto_armor_auto_dodge(Defense, Health, DamageModifier).

-spec finalize_to_omnimods
   (
      non_neg_integer(),
      factors(),
      proto_armor()
   )
   -> shr_omnimods:type().
finalize_to_omnimods (MovementPoints, Factors, ProtoArmor) ->
   shr_omnimods:new
   (
      [
         {movement_points, MovementPoints},
         {
         },
         {
         },
         {
         },
         {
         }
      ],
      [],
      [
      ]
   ).
