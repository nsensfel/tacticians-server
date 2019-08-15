-module(blc_armor).

-include("base_attributes.hrl").

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
      health :: non_neg_integer(),
      damage_modifier :: non_neg_integer(),
      dodge :: non_neg_integer(),
      mvt_points :: non_neg_integer(),
      defense :: list(blc_damage_type:entry()),
      defense_coef :: list(blc_damage_type:coefficient()),
      defense_score :: non_neg_integer()
   }
).

-opaque proto_armor() :: #proto_armor{}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export_type([proto_armor/0]).

% FIXME: quick debug
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
      get_spendable_points/0
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

-spec increase_movement_points_by
   (
      non_neg_integer(),
      proto_armor()
   )
   -> {proto_armor(), non_neg_integer()}.
increase_movement_points_by (Amount, Armor) ->
   NewMvtPoints = Armor#proto_armor.mvt_points + Amount,
   case (NewMvtPoints > ?ATTRIBUTE_MOVEMENT_POINTS_MAX) of
      true ->
         {
            Armor#proto_armor{ mvt_points = ?ATTRIBUTE_MOVEMENT_POINTS_MAX },
            (
               (?ATTRIBUTE_MOVEMENT_POINTS_MAX - Armor#proto_armor.mvt_points)
               * ?ATTRIBUTE_MOVEMENT_POINTS_COST
            )
         };

      false ->
         {
            Armor#proto_armor{ mvt_points = NewMvtPoints },
            (Amount * ?ATTRIBUTE_MOVEMENT_POINTS_COST)
         }
   end.

-spec increase_defense_score_by
   (
      non_neg_integer(),
      proto_armor()
   )
   -> {proto_armor(), non_neg_integer()}.
increase_defense_score_by (Amount, Armor) ->
   NewDefenseScore = (Armor#proto_armor.defense_score + Amount),
   case (NewDefenseScore > ?ATTRIBUTE_DEFENSE_SCORE_MAX) of
      true ->
         {
            Armor#proto_armor
            {
               defense_score = ?ATTRIBUTE_DEFENSE_SCORE_MAX,
               defense =
                  blc_damage_type:generate_entries_from_score
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
                  blc_damage_type:generate_entries_from_score
                  (
                     NewDefenseScore,
                     Armor#proto_armor.defense_coef
                  )
            },
            (Amount * ?ATTRIBUTE_DEFENSE_SCORE_COST)
         }
   end.

-spec set_defense_coefficients
   (
      list(blc_damage_type:coefficient()),
      proto_armor()
   )
   -> proto_armor().
set_defense_coefficients (Coefficients, Armor) ->
   {Result, 0} =
      increase_defense_score_by
      (
         0,
         Armor#proto_armor
         {
            defense_coef = blc_damage_type:sort_entries(Coefficients)
         }
      ),

   Result.

-spec new (list(blc_damage_type:coefficient())) -> proto_armor().
new (Coefficients) ->
   {Result, _DefenseScoreIncreaseCost} =
      increase_defense_score_by
      (
         ?ATTRIBUTE_DEFENSE_SCORE_MIN,
         #proto_armor
         {
            health = ?ATTRIBUTE_HEALTH_MIN,
            damage_modifier = ?ATTRIBUTE_DAMAGE_MODIFIER_MIN,
            dodge = ?ATTRIBUTE_DODGE_CHANCE_MIN,
            mvt_points = ?ATTRIBUTE_MOVEMENT_POINTS_MIN,
            defense = [],
            defense_coef = blc_damage_type:sort_entries(Coefficients),
            defense_score = 0
         }
      ),

   Result.

-spec increase_health_for
   (
      non_neg_integer(),
      proto_armor()
   )
   -> {proto_armor(), non_neg_integer()}.
increase_health_for (GivenPoints, Armor) ->
   AmountOfIncrease = trunc(GivenPoints / ?ATTRIBUTE_HEALTH_COST),
   {Result, SpentPoints} = increase_health_by(AmountOfIncrease, Armor),
   {Result, (GivenPoints - SpentPoints)}.

-spec increase_damage_modifier_for
   (
      non_neg_integer(),
      proto_armor()
   )
   -> {proto_armor(), non_neg_integer()}.
increase_damage_modifier_for (GivenPoints, Armor) ->
   AmountOfIncrease = trunc(GivenPoints / ?ATTRIBUTE_DAMAGE_MODIFIER_COST),
   {Result, SpentPoints} = increase_damage_modifier_by(AmountOfIncrease, Armor),
   {Result, (GivenPoints - SpentPoints)}.

-spec increase_dodge_chance_for
   (
      non_neg_integer(),
      proto_armor()
   )
   -> {proto_armor(), non_neg_integer()}.
increase_dodge_chance_for (GivenPoints, Armor) ->
   AmountOfIncrease = trunc(GivenPoints / ?ATTRIBUTE_DODGE_CHANCE_COST),
   {Result, SpentPoints} = increase_dodge_chance_by(AmountOfIncrease, Armor),
   {Result, (GivenPoints - SpentPoints)}.

-spec increase_movement_points_for
   (
      non_neg_integer(),
      proto_armor()
   )
   -> {proto_armor(), non_neg_integer()}.
increase_movement_points_for (GivenPoints, Armor) ->
   AmountOfIncrease = trunc(GivenPoints / ?ATTRIBUTE_MOVEMENT_POINTS_COST),
   {Result, SpentPoints} = increase_movement_points_by(AmountOfIncrease, Armor),
   {Result, (GivenPoints - SpentPoints)}.

-spec increase_defense_score_for
   (
      non_neg_integer(),
      proto_armor()
   )
   -> {proto_armor(), non_neg_integer()}.
increase_defense_score_for (GivenPoints, Armor) ->
   AmountOfIncrease = trunc(GivenPoints / ?ATTRIBUTE_DEFENSE_SCORE_COST),
   {Result, SpentPoints} = increase_defense_score_by(AmountOfIncrease, Armor),
   {Result, (GivenPoints - SpentPoints)}.


-spec get_spendable_points () -> non_neg_integer().
get_spendable_points () -> ?SPENDABLE_ARMOR_POINTS.
