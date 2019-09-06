-module(blc_glyph).

-include("tacticians/attributes.hrl").
-include("tacticians/damage_types.hrl").

-define(SPENDABLE_GLYPH_POINTS, 100).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record
(
   proto_glyph,
   {
      remaining_positive_points :: non_neg_integer(),
      points_balance :: integer(),
      omnimods :: shr_omnimods:type(),
      defense_coef :: list(blc_damage_type:coefficient()),
      attack_coef :: list(blc_damage_type:coefficient()),
      defense_score :: non_neg_integer(),
      defense_sign :: integer(),
      attack_score :: non_neg_integer(),
      attack_sign :: integer()
   }
).

-opaque type() :: #proto_glyph{}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export_type([type/0]).

-export
(
   [
      increase_attribute_by/3,
      decrease_attribute_by/4,
      increase_attribute_for/3,
      decrease_attribute_for/4,
      set_attack_coefficients/2,
      set_defense_coefficients/2
   ]
).

-export
(
   [
      new/2,
      new/4,
      generate/0,
      get_remaining_positive_points/1,
      get_points_balance/1,
      export/1
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec attribute_coefficent
   (
      shr_attributes:meta_enum(),
      shr_attributes:meta_enum()
   )
   -> ({ok, float()} | error).
attribute_coefficent (Attr0, Attr1) ->
   case {Attr0, Attr1} of
      {?ATTRIBUTE_ACCURACY, ?ATTRIBUTE_CRITICAL_HIT_CHANCE} -> {ok, 0.75};
      {?ATTRIBUTE_ACCURACY, ?ATTRIBUTE_DODGE_CHANCE} -> {ok, 0.75};
      {?ATTRIBUTE_ACCURACY, ?ATTRIBUTE_HEALTH} -> {ok, 1.00};
      {?ATTRIBUTE_ACCURACY, ?ATTRIBUTE_MOVEMENT_POINTS} -> {ok, 1.00};

      {?ATTRIBUTE_CRITICAL_HIT_CHANCE, ?ATTRIBUTE_DOUBLE_HIT_CHANCE} ->
         {ok, 0.75};
      {?ATTRIBUTE_CRITICAL_HIT_CHANCE, ?ATTRIBUTE_DAMAGE_MODIFIER} ->
         {ok, 0.75};
      {?ATTRIBUTE_CRITICAL_HIT_CHANCE, ?ATTRIBUTE_DODGE_CHANCE} -> {ok, 1.00};
      {?ATTRIBUTE_CRITICAL_HIT_CHANCE, ?ATTRIBUTE_PARRY_CHANCE} -> {ok, 1.00};

      {?ATTRIBUTE_DOUBLE_HIT_CHANCE, ?ATTRIBUTE_ACCURACY} -> {ok, 0.75};
      {?ATTRIBUTE_DOUBLE_HIT_CHANCE, ?ATTRIBUTE_DAMAGE_MODIFIER} -> {ok, 0.75};
      {?ATTRIBUTE_DOUBLE_HIT_CHANCE, ?ATTRIBUTE_HEALTH} -> {ok, 1.00};
      {?ATTRIBUTE_DOUBLE_HIT_CHANCE, ?ATTRIBUTE_MOVEMENT_POINTS} -> {ok, 1.00};

      {?ATTRIBUTE_DAMAGE_MODIFIER, ?ATTRIBUTE_DODGE_CHANCE} -> {ok, 1.00};
      {?ATTRIBUTE_DAMAGE_MODIFIER, ?ATTRIBUTE_HEALTH} -> {ok, 0.75};
      {?ATTRIBUTE_DAMAGE_MODIFIER, ?ATTRIBUTE_MOVEMENT_POINTS} -> {ok, 1.00};
      {?ATTRIBUTE_DAMAGE_MODIFIER, ?ATTRIBUTE_DEFENSE_SCORE} -> {ok, 0.75};

      {?ATTRIBUTE_DODGE_CHANCE, ?ATTRIBUTE_HEALTH} -> {ok, 0.75};
      {?ATTRIBUTE_DODGE_CHANCE, ?ATTRIBUTE_PARRY_CHANCE} -> {ok, 0.75};

      {?ATTRIBUTE_HEALTH, ?ATTRIBUTE_MOVEMENT_POINTS} -> {ok, 0.75};
      {?ATTRIBUTE_HEALTH, ?ATTRIBUTE_ATTACK_SCORE} -> {ok, 0.75};

      {?ATTRIBUTE_MOVEMENT_POINTS, ?ATTRIBUTE_DODGE_CHANCE} -> {ok, 0.75};
      {?ATTRIBUTE_MOVEMENT_POINTS, ?ATTRIBUTE_ATTACK_SCORE} -> {ok, 0.75};
      {?ATTRIBUTE_MOVEMENT_POINTS, ?ATTRIBUTE_DEFENSE_SCORE} -> {ok, 1.00};

      {?ATTRIBUTE_PARRY_CHANCE, ?ATTRIBUTE_ACCURACY} -> {ok, 0.75};
      {?ATTRIBUTE_PARRY_CHANCE, ?ATTRIBUTE_MOVEMENT_POINTS} -> {ok, 0.75};
      {?ATTRIBUTE_PARRY_CHANCE, ?ATTRIBUTE_ATTACK_SCORE} -> {ok, 1.00};
      {?ATTRIBUTE_PARRY_CHANCE, ?ATTRIBUTE_DEFENSE_SCORE} -> {ok, 0.75};

      {?ATTRIBUTE_ATTACK_SCORE, ?ATTRIBUTE_CRITICAL_HIT_CHANCE} -> {ok, 0.75};

      {?ATTRIBUTE_DEFENSE_SCORE, ?ATTRIBUTE_DOUBLE_HIT_CHANCE} -> {ok, 0.75};

      _ -> error
   end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec increase_attribute_by
   (
      shr_attributes:meta_enum(),
      non_neg_integer(),
      type()
   )
   -> ({ok, type()} | blc_error:type()).
increase_attribute_by (?ATTRIBUTE_ATTACK_SCORE, S0Amount, Glyph) ->
   S0NewAttackScore = (Glyph#proto_glyph.attack_score + S0Amount),
   {S1NewAttackScore, S1Amount} =
      case (S0NewAttackScore > ?ATTRIBUTE_ATTACK_SCORE_MAX) of
         true ->
            {
               ?ATTRIBUTE_ATTACK_SCORE_MAX,
               (?ATTRIBUTE_ATTACK_SCORE_MAX - Glyph#proto_glyph.attack_score)
            };

         false -> {S0NewAttackScore, S0Amount}
      end,

   Cost = (S1Amount * ?ATTRIBUTE_ATTACK_SCORE_COST),

   if
      (Glyph#proto_glyph.attack_sign == -1) -> {error, incompatible};
      (Cost > (Glyph#proto_glyph.remaining_positive_points)) ->
         {error, balance, Glyph#proto_glyph.remaining_positive_points, Cost};

      true ->
         {
            ok,
            Glyph#proto_glyph
            {
               attack_score = S1NewAttackScore,
               attack_sign = 1,
               omnimods =
                  shr_omnimods:set_attack_modifiers
                  (
                     blc_damage_type:generate_entries_from_score
                     (
                        S1NewAttackScore,
                        Glyph#proto_glyph.attack_coef
                     ),
                     Glyph#proto_glyph.omnimods
                  ),
               remaining_positive_points =
                  (Glyph#proto_glyph.remaining_positive_points - Cost),
               points_balance = (Glyph#proto_glyph.points_balance - Cost)
            }
         }
   end;
increase_attribute_by (?ATTRIBUTE_DEFENSE_SCORE, S0Amount, Glyph) ->
   S0NewDefenseScore = (Glyph#proto_glyph.defense_score + S0Amount),
   {S1NewDefenseScore, S1Amount} =
      case (S0NewDefenseScore > ?ATTRIBUTE_DEFENSE_SCORE_MAX) of
         true ->
            {
               ?ATTRIBUTE_DEFENSE_SCORE_MAX,
               (?ATTRIBUTE_DEFENSE_SCORE_MAX - Glyph#proto_glyph.defense_score)
            };

         false -> {S0NewDefenseScore, S0Amount}
      end,

   Cost = (S1Amount * ?ATTRIBUTE_DEFENSE_SCORE_COST),

   if
      (Glyph#proto_glyph.defense_sign == -1) -> {error, incompatible};
      (Cost > (Glyph#proto_glyph.remaining_positive_points)) ->
         {error, balance, Glyph#proto_glyph.remaining_positive_points, Cost};

      true ->
         {
            ok,
            Glyph#proto_glyph
            {
               defense_score = S1NewDefenseScore,
               defense_sign = 1,
               omnimods =
                  shr_omnimods:set_defense_modifiers
                  (
                     blc_damage_type:generate_entries_from_score
                     (
                        S1NewDefenseScore,
                        Glyph#proto_glyph.defense_coef
                     ),
                     Glyph#proto_glyph.omnimods
                  ),
               remaining_positive_points =
                  (Glyph#proto_glyph.remaining_positive_points - Cost),
               points_balance = (Glyph#proto_glyph.points_balance - Cost)
            }
         }
   end;
increase_attribute_by (Attribute, S0Amount, Glyph) ->
   {_AttMin, _AttDef, AttMax, AttCost} = blc_attribute:get_info(Attribute),
   CurrentOmnimods = Glyph#proto_glyph.omnimods,
   CurrentValue =
      shr_omnimods:get_attribute_modifier(Attribute, CurrentOmnimods),

   S1Amount =
      case ((CurrentValue + S0Amount) > AttMax) of
         true -> (AttMax - CurrentValue);
         false -> S0Amount
      end,

   Cost = (S1Amount * AttCost),

   if
      (CurrentValue < 0) -> {error, incompatible};
      (Cost > (Glyph#proto_glyph.remaining_positive_points)) ->
         {error, balance, Glyph#proto_glyph.remaining_positive_points, Cost};

      true ->
         {
            ok,
            Glyph#proto_glyph
            {
               omnimods =
                  shr_omnimods:mod_attribute_modifier
                  (
                     Attribute,
                     S1Amount,
                     CurrentOmnimods
                  ),
               remaining_positive_points =
                  (Glyph#proto_glyph.remaining_positive_points - Cost),
               points_balance = (Glyph#proto_glyph.points_balance - Cost)
            }
         }
   end.

-spec decrease_attribute_by
   (
      shr_attributes:meta_enum(),
      non_neg_integer(),
      float(),
      type()
   )
   -> ({ok, type()} | blc_error:type()).
decrease_attribute_by
(
   ?ATTRIBUTE_ATTACK_SCORE,
   Amount,
   PointsMultiplier,
   Glyph
) ->
   NewAttackScore = (Glyph#proto_glyph.attack_score + Amount),

   Cost =
      trunc
      (
         (Amount * ?ATTRIBUTE_ATTACK_SCORE_COST)
         * PointsMultiplier
      ),

   if
      (Glyph#proto_glyph.attack_sign == -1) -> {error, incompatible};
      true ->
         {
            ok,
            Glyph#proto_glyph
            {
               attack_score = NewAttackScore,
               attack_sign = -1,
               omnimods =
                  shr_omnimods:set_attack_modifiers
                  (
                     lists:map
                     (
                        fun ({Name, Value}) -> {Name, (-1 * Value)} end,
                        blc_damage_type:generate_entries_from_score
                        (
                           NewAttackScore,
                           Glyph#proto_glyph.attack_coef
                        )
                     ),
                     Glyph#proto_glyph.omnimods
                  ),
               points_balance = (Glyph#proto_glyph.points_balance + Cost)
            }
         }
   end;
decrease_attribute_by
(
   ?ATTRIBUTE_DEFENSE_SCORE,
   Amount,
   PointsMultiplier,
   Glyph
) ->
   NewDefenseScore = (Glyph#proto_glyph.defense_score + Amount),

   Cost =
      trunc
      (
         (Amount * ?ATTRIBUTE_DEFENSE_SCORE_COST)
         * PointsMultiplier
      ),

   if
      (Glyph#proto_glyph.defense_sign == -1) -> {error, incompatible};
      true ->
         {
            ok,
            Glyph#proto_glyph
            {
               defense_score = NewDefenseScore,
               defense_sign = -1,
               omnimods =
                  shr_omnimods:set_defense_modifiers
                  (
                     lists:map
                     (
                        fun ({Name, Value}) -> {Name, (-1 * Value)} end,
                        blc_damage_type:generate_entries_from_score
                        (
                           NewDefenseScore,
                           Glyph#proto_glyph.defense_coef
                        )
                     ),
                     Glyph#proto_glyph.omnimods
                  ),
               points_balance = (Glyph#proto_glyph.points_balance + Cost)
            }
         }
   end;
decrease_attribute_by (Attribute, Amount, PointsMultiplier, Glyph) ->
   {_AttMin, _AttDef, _AttMax, AttCost} = blc_attribute:get_info(Attribute),
   CurrentOmnimods = Glyph#proto_glyph.omnimods,
   CurrentValue =
      shr_omnimods:get_attribute_modifier(Attribute, CurrentOmnimods),

   Cost = trunc((Amount * AttCost) * PointsMultiplier),

   if
      (CurrentValue > 0) -> {error, incompatible};

      true ->
         {
            ok,
            Glyph#proto_glyph
            {
               omnimods =
                  shr_omnimods:mod_attribute_modifier
                  (
                     Attribute,
                     (-1 * Amount),
                     CurrentOmnimods
                  ),
               points_balance = (Glyph#proto_glyph.points_balance + Cost)
            }
         }
   end.

-spec set_attack_coefficients
   (
      list(blc_damage_type:coefficient()),
      type()
   )
   -> type().
set_attack_coefficients (Coefficients, Glyph) ->
   NewGlyph =
      Glyph#proto_glyph
      {
         attack_coef = blc_damage_type:sort_entries(Coefficients)
      },

   case (NewGlyph#proto_glyph.attack_sign) of
      0 -> NewGlyph;
      1 ->
         NewGlyph#proto_glyph
         {
            omnimods =
               shr_omnimods:set_attack_modifiers
               (
                  blc_damage_type:generate_entries_from_score
                  (
                     NewGlyph#proto_glyph.attack_score,
                     NewGlyph#proto_glyph.attack_coef
                  ),
                  NewGlyph#proto_glyph.omnimods
               )
         };
      -1 ->
         NewGlyph#proto_glyph
         {
            omnimods =
               shr_omnimods:set_attack_modifiers
               (
                  lists:map
                  (
                     fun ({Name, Value}) -> {Name, (-1 * Value)} end,
                     blc_damage_type:generate_entries_from_score
                     (
                        NewGlyph#proto_glyph.attack_score,
                        NewGlyph#proto_glyph.attack_coef
                     )
                  ),
                  NewGlyph#proto_glyph.omnimods
               )
         }
   end.

-spec set_defense_coefficients
   (
      list(blc_damage_type:coefficient()),
      type()
   )
   -> type().
set_defense_coefficients (Coefficients, Glyph) ->
   NewGlyph =
      Glyph#proto_glyph
      {
         defense_coef = blc_damage_type:sort_entries(Coefficients)
      },

   case (NewGlyph#proto_glyph.defense_sign) of
      0 -> NewGlyph;
      1 ->
         NewGlyph#proto_glyph
         {
            omnimods =
               shr_omnimods:set_defense_modifiers
               (
                  blc_damage_type:generate_entries_from_score
                  (
                     NewGlyph#proto_glyph.defense_score,
                     NewGlyph#proto_glyph.defense_coef
                  ),
                  NewGlyph#proto_glyph.omnimods
               )
         };
      -1 ->
         NewGlyph#proto_glyph
         {
            omnimods =
               shr_omnimods:set_defense_modifiers
               (
                  lists:map
                  (
                     fun ({Name, Value}) -> {Name, (-1 * Value)} end,
                     blc_damage_type:generate_entries_from_score
                     (
                        NewGlyph#proto_glyph.defense_score,
                        NewGlyph#proto_glyph.defense_coef
                     )
                  ),
                  NewGlyph#proto_glyph.omnimods
               )
         }
   end.

-spec new
   (
      list(blc_damage_type:coefficient()),
      list(blc_damage_type:coefficient())
   )
   -> type().
new (AttackCoefficients, DefenseCoefficients) ->
   #proto_glyph
   {
      remaining_positive_points = ?SPENDABLE_GLYPH_POINTS,
      points_balance = 0,
      omnimods = shr_omnimods:new(),
      attack_coef = blc_damage_type:sort_entries(AttackCoefficients),
      attack_score = 0,
      attack_sign = 0,
      defense_coef = blc_damage_type:sort_entries(DefenseCoefficients),
      defense_score = 0,
      defense_sign = 0
   }.

-spec new
   (
      list(blc_damage_type:coefficient()),
      list(blc_damage_type:coefficient()),
      shr_attributes:meta_enum(),
      shr_attributes:meta_enum()
   )
   -> ({ok, type()} | error).
new (AttackCoefficients, DefenseCoefficients, IncreasedAtt, DecreasedAtt) ->
   case attribute_coefficent(IncreasedAtt, DecreasedAtt) of
      error -> error;
      {ok, PointsReduction} ->
         PointsMultiplier = (2.0 - PointsReduction),
         S0Result = new(AttackCoefficients, DefenseCoefficients),
         case
            decrease_attribute_for
            (
               DecreasedAtt,
               get_remaining_positive_points(S0Result),
               PointsMultiplier,
               S0Result
            )
         of
            {ok, S1Result} ->
               case
                  increase_attribute_for
                  (
                     IncreasedAtt,
                     get_points_balance(S1Result),
                     S1Result
                  )
               of
                  {ok, S2Result} -> {ok, S2Result};
                  _ -> error
               end;

            _ -> error
         end
   end.

-spec increase_attribute_for
   (
      shr_attributes:meta_enum(),
      non_neg_integer(),
      type()
   )
   -> ({ok, type} | blc_error:type()).
increase_attribute_for (Attribute, GivenPoints, Glyph) ->
   {_AttMin, _AttDef, _AttMax, AttCost} = blc_attribute:get_info(Attribute),
   AmountOfIncrease = trunc(GivenPoints / AttCost),
   increase_attribute_by(Attribute, AmountOfIncrease, Glyph).

-spec decrease_attribute_for
   (
      shr_attributes:meta_enum(),
      non_neg_integer(),
      float(),
      type()
   )
   -> ({ok, type} | blc_error:type()).
decrease_attribute_for (Attribute, GivenPoints, PointsMultiplier, Glyph) ->
   {_AttMin, _AttDef, _AttMax, AttCost} = blc_attribute:get_info(Attribute),
   AmountOfDecrease =
      trunc(GivenPoints / (PointsMultiplier * AttCost)),

   decrease_attribute_by(Attribute, AmountOfDecrease, PointsMultiplier, Glyph).

-spec get_remaining_positive_points (type()) -> non_neg_integer().
get_remaining_positive_points (Glyph) ->
   Glyph#proto_glyph.remaining_positive_points.

-spec get_points_balance (type()) -> non_neg_integer().
get_points_balance (Glyph) ->
   Glyph#proto_glyph.points_balance.

-spec generate () -> list(type()).
generate () ->
   Attributes =
      [
         ?ATTRIBUTE_ACCURACY,
         ?ATTRIBUTE_ATTACK_SCORE,
         ?ATTRIBUTE_CRITICAL_HIT_CHANCE,
         ?ATTRIBUTE_DAMAGE_MODIFIER,
         ?ATTRIBUTE_DEFENSE_SCORE,
         ?ATTRIBUTE_DODGE_CHANCE,
         ?ATTRIBUTE_DOUBLE_HIT_CHANCE,
         ?ATTRIBUTE_HEALTH,
         ?ATTRIBUTE_MOVEMENT_POINTS,
         ?ATTRIBUTE_PARRY_CHANCE
      ],

   DamageTypeDistributions =
      lists:map
      (
         fun ([A, B, C]) ->
            [
               {?DAMAGE_TYPE_SLASH, A},
               {?DAMAGE_TYPE_PIERCE, B},
               {?DAMAGE_TYPE_BLUNT, C}
            ]
         end,
         blc_distribution:generate(3, 25)
      ),

   S0Result =
      shr_lists_util:product
      (
         fun (IncreasedAttribute, DecreasedAttribute) ->
            case {IncreasedAttribute, DecreasedAttribute} of
               {A, B} when
                  (
                     (
                        (A =:= ?ATTRIBUTE_DEFENSE_SCORE)
                        or (B =:= ?ATTRIBUTE_ATTACK_SCORE)
                     )
                     and
                     (
                        (A =:= ?ATTRIBUTE_ATTACK_SCORE)
                        or (B =:= ?ATTRIBUTE_DEFENSE_SCORE)
                     )
                  ) ->
                     shr_lists_util:product
                     (
                        fun (AttackCoefs, DefenseCoefs) ->
                           new
                           (
                              AttackCoefs,
                              DefenseCoefs,
                              IncreasedAttribute,
                              DecreasedAttribute
                           )
                        end,
                        DamageTypeDistributions,
                        DamageTypeDistributions
                     );

               {A, B} when
                     (
                        (A =:= ?ATTRIBUTE_DEFENSE_SCORE)
                        or (B =:= ?ATTRIBUTE_DEFENSE_SCORE)
                     ) ->
                  lists:map
                  (
                     fun (DefenseCoefs) ->
                        new
                        (
                           [],
                           DefenseCoefs,
                           IncreasedAttribute,
                           DecreasedAttribute
                        )
                     end,
                     DamageTypeDistributions
                  );

               {A, B} when
                     (
                        (A =:= ?ATTRIBUTE_ATTACK_SCORE)
                        or (B =:= ?ATTRIBUTE_ATTACK_SCORE)
                     ) ->
                  lists:map
                  (
                     fun (AttackCoefs) ->
                        new
                        (
                           AttackCoefs,
                           [],
                           IncreasedAttribute,
                           DecreasedAttribute
                        )
                     end,
                     DamageTypeDistributions
                  );

               _ -> [new([], [], IncreasedAttribute, DecreasedAttribute)]
            end
         end,
         Attributes,
         Attributes
      ),

   lists:filtermap
   (
      fun (Candidate) ->
         case Candidate of
            {ok, Result} -> {true, Result};
            _ -> false
         end
      end,
      lists:flatten(S0Result)
   ).

-spec export (type()) -> list().
export (Glyph) -> shr_omnimods:export(Glyph#proto_glyph.omnimods).
