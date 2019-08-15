-module(blc_armor).

-include("../../../include/base_attributes.hrl").

-define
(
   ATTRIBUTE_ARMOR_POINTS,
   (
      (
         ?ATTRIBUTE_DAMAGE_MODIFIER_COST
         * (?ATTRIBUTE_DAMAGE_MODIFIER_BASE - ?ATTRIBUTE_DAMAGE_MODIFIER_MIN)
      )
      +
      (
         ?ATTRIBUTE_MOVEMENT_POINTS_COST
         * (?ATTRIBUTE_MOVEMENT_POINTS_BASE - ?ATTRIBUTE_MOVEMENT_POINTS_MIN)
      )
      +
      (
         ?ATTRIBUTE_HEALTH_COST
         * (?ATTRIBUTE_HEALTH_BASE - ?ATTRIBUTE_HEALTH_MIN)
      )
      +
      (
         ?ATTRIBUTE_DEFENSE_SCORE_COST
         * (?ATTRIBUTE_DEFENSE_SCORE_BASE - ?ATTRIBUTE_DEFENSE_SCORE_MIN)
      )
      + (?ATTRIBUTE_DODGE_COST * (?ATTRIBUTE_DODGE_BASE - ?ATTRIBUTE_DODGE_MIN))
   )
).

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
      defense_coef :: list(defense_entry()),
      defense_score :: non_neg_integer()
   }
).

-opaque proto_armor() :: #proto_armor{}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export_type([proto_armor/0]).

% FIXME: quick debug
-compile(export_all).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec sort_defense_entries (list(defense_entry())) -> list(defense_entry()).
sort_defense_entries (Entries) ->
   lists:sort
   (
      fun ({_NameA, ValueA}, {_NameB, ValueB}) ->
         (ValueA >= ValueB)
      end,
      Entries
   ).

-spec calc_defense_score (list(defense_entry())) -> non_neg_integer().
calc_defense_score (Defense) ->
   {_LastIndex, Result} =
      lists:foldl
      (
         fun ({_NameA, ValueA}, {Index, Current}) ->
            {(Index + 1), (Current + (Index * ValueA))}
         end,
         {1, 0},
         Defense
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
   {S1DescSortedDefense, {_EndIndex, EndModifier}} =
      lists:mapfoldl
      (
         fun ({Name, S0Value}, {Index, RemainingModifier}) ->
            case ((RemainingModifier >= Index) and (S0Value > 0)) of
               true ->
                  {
                     {Name, (S0Value + Mod)},
                     {
                        (Index + 1),
                        RemainingModifier - Index
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

-spec generate_defense_worth
   (
      non_neg_integer(),
      list(defense_entry())
   )
   -> list(defense_entry()).
generate_defense_worth (TargetScore, SortedRatios) ->
   [{T0, V0}, {T1, V1}, {T2, V2}] = SortedRatios,
   Distribution = ((V0 + 2 * V1 + 3 * V2) / 100),
   Base = TargetScore / Distribution,
   UnderperformingDefense =
      [
         {T0, trunc(Base * (V0/100))},
         {T1, trunc(Base * (V1/100))},
         {T2, trunc(Base * (V2/100))}
      ],
   MissingScore = TargetScore - calc_defense_score(UnderperformingDefense),
   case (MissingScore >= 0) of
      true ->
         apply_defense_score_modifier
         (
            MissingScore,
            1,
            UnderperformingDefense
         );

      false ->
         apply_defense_score_modifier
         (
            (-1 * MissingScore),
            -1,
            UnderperformingDefense
         )
   end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec increase_health_by
   (
      non_neg_integer(),
      proto_armor()
   )
   -> {proto_armor(), non_neg_integer()}.
increase_health_by (Amount, Armor) ->
   NewHealth = Armor#proto_armor.health + Amount,
   case (NewHealth > ?ATTRIBUTE_HEALTH_MAX) of
      true ->
         {
            Armor#proto_armor{ health = ?ATTRIBUTE_HEALTH_MAX },
            (
               (?ATTRIBUTE_HEALTH_MAX - Armor#proto_armor.health)
               * ?ATTRIBUTE_HEALTH_COST
            )
         };

      false ->
         {
            Armor#proto_armor{ health = NewHealth },
            (Amount * ?ATTRIBUTE_HEALTH_COST)
         }
   end.

-spec increase_damage_modifier_by
   (
      non_neg_integer(),
      proto_armor()
   )
   -> {proto_armor(), non_neg_integer()}.
increase_damage_modifier_by (Amount, Armor) ->
   NewDamageModifier = Armor#proto_armor.damage_modifier + Amount,
   case (NewDamageModifier > ?ATTRIBUTE_DAMAGE_MODIFIER_MAX) of
      true ->
         {
            Armor#proto_armor
            {
               damage_modifier = ?ATTRIBUTE_DAMAGE_MODIFIER_MAX
            },
            (
               (
                  ?ATTRIBUTE_DAMAGE_MODIFIER_MAX
                  - Armor#proto_armor.damage_modifier
               )
               * ?ATTRIBUTE_DAMAGE_MODIFIER_COST
            )
         };

      false ->
         {
            Armor#proto_armor{ damage_modifier = NewDamageModifier },
            (Amount * ?ATTRIBUTE_DAMAGE_MODIFIER_COST)
         }
   end.

-spec increase_dodge_chance_by
   (
      non_neg_integer(),
      proto_armor()
   )
   -> {proto_armor(), non_neg_integer()}.
increase_dodge_chance_by (Amount, Armor) ->
   NewDodgeChance = Armor#proto_armor.dodge + Amount,
   case (NewDodgeChance > ?ATTRIBUTE_DODGE_CHANCE_MAX) of
      true ->
         {
            Armor#proto_armor{ dodge = ?ATTRIBUTE_DODGE_CHANCE_MAX },
            (
               (?ATTRIBUTE_DODGE_CHANCE_MAX - Armor#proto_armor.dodge)
               * ?ATTRIBUTE_DODGE_CHANCE_COST
            )
         };

      false ->
         {
            Armor#proto_armor{ dodge = NewDodgeChance },
            (Amount * ?ATTRIBUTE_DODGE_CHANCE_COST)
         }
   end.

-spec increase_defense_score_by
   (
      non_neg_integer(),
      proto_armor()
   )
   -> {proto_armor(), non_neg_integer()}.
increase_defense_score_by (Amount, Armor) ->
   NewDefenseScore = Armor#proto_armor.defense_score + Amount,
   case (NewDefenseScore > ?ATTRIBUTE_DEFENSE_SCORE_MAX) of
      true ->
         {
            Armor#proto_armor
            {
               defense_score = ?ATTRIBUTE_DEFENSE_SCORE_MAX,
               defense =
                  generate_defense_worth
                  (
                     NewDefenseScore,
                     Armor#proto_armor.defense_coef
                  )
            },
            (
               (?ATTRIBUTE_DEFENSE_SCORE_MAX - Armor#proto_armor.defense_score)
               * Amount
            )
         };

      false ->
         {
            Armor#proto_armor
            {
               defense_score = NewDefenseScore,
               defense =
                  generate_defense_worth
                  (
                     NewDefenseScore,
                     Armor#proto_armor.defense_coef
                  )
            },
            (Amount * ?ATTRIBUTE_DEFENSE_SCORE_COST)
         }
   end.
