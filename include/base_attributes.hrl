-define(ATTRIBUTE_DAMAGE_MODIFIER_MIN,       0).
-define(ATTRIBUTE_DAMAGE_MODIFIER_MAX,       300).
-define(ATTRIBUTE_DAMAGE_MODIFIER_DEFAULT,   100).
-define(ATTRIBUTE_DAMAGE_MODIFIER_COST,      1).
-define
(
   ATTRIBUTE_DAMAGE_MODIFIER_MAX_POINTS,
   (
      (?ATTRIBUTE_DAMAGE_MODIFIER_MAX - ?ATTRIBUTE_DAMAGE_MODIFIER_MIN)
      * ?ATTRIBUTE_DAMAGE_MODIFIER_COST
).

-define(ATTRIBUTE_MOVEMENT_POINTS_MIN,       8).
-define(ATTRIBUTE_MOVEMENT_POINTS_MAX,       200).
-define(ATTRIBUTE_MOVEMENT_POINTS_DEFAULT,   32).
-define(ATTRIBUTE_MOVEMENT_POINTS_COST,      1).
-define
(
   ATTRIBUTE_MOVEMENT_POINTS_MAX_POINTS,
   (
      (?ATTRIBUTE_MOVEMENT_POINTS_MAX - ?ATTRIBUTE_MOVEMENT_POINTS_MIN)
      * ?ATTRIBUTE_MOVEMENT_POINTS_COST
   )
).

-define(ATTRIBUTE_HEALTH_MIN,       1).
-define(ATTRIBUTE_HEALTH_MAX,       500).
-define(ATTRIBUTE_HEALTH_DEFAULT,   100).
-define(ATTRIBUTE_HEALTH_COST,      1).
-define
(
   ATTRIBUTE_HEALTH_MAX_POINTS,
   ((?ATTRIBUTE_HEALTH_MAX - ?ATTRIBUTE_HEALTH_MIN) * ?ATTRIBUTE_HEALTH_COST)
).

-define(ATTRIBUTE_DODGE_CHANCE_MIN,     0).
-define(ATTRIBUTE_DODGE_CHANCE_MAX,     175).
-define(ATTRIBUTE_DODGE_CHANCE_DEFAULT, 50).
-define(ATTRIBUTE_DODGE_CHANCE_COST,    1).
-define
(
   ATTRIBUTE_DODGE_CHANCE_MAX_POINTS,
   (
      (?ATTRIBUTE_DODGE_CHANCE_MAX - ?ATTRIBUTE_DODGE_CHANCE_MIN)
      * ?ATTRIBUTE_DODGE_CHANCE_COST
   )
).

-define(ATTRIBUTE_PARRY_CHANCE_MIN,     0).
-define(ATTRIBUTE_PARRY_CHANCE_MAX,     100).
-define(ATTRIBUTE_PARRY_CHANCE_DEFAULT, 5).
-define(ATTRIBUTE_PARRY_CHANCE_COST,    1).
-define
(
   ATTRIBUTE_PARRY_CHANCE_MAX_POINTS,
   (
      (?ATTRIBUTE_PARRY_CHANCE_MAX - ?ATTRIBUTE_PARRY_CHANCE_MIN)
      * ?ATTRIBUTE_PARRY_CHANCE_COST
   )
).

-define(ATTRIBUTE_ACCURACY_MIN,     0).
-define(ATTRIBUTE_ACCURACY_MAX,     100).
-define(ATTRIBUTE_ACCURACY_DEFAULT, 50).
-define(ATTRIBUTE_ACCURACY_COST,    1).
-define
(
   ATTRIBUTE_ACCURACY_MAX_POINTS,
   (
      (?ATTRIBUTE_ACCURACY_MAX - ?ATTRIBUTE_ACCURACY_MIN)
      * ?ATTRIBUTE_ACCURACY_COST
   )
).

-define(ATTRIBUTE_DOUBLE_HIT_CHANCE_MIN,     0).
-define(ATTRIBUTE_DOUBLE_HIT_CHANCE_MAX,     100).
-define(ATTRIBUTE_DOUBLE_HIT_CHANCE_DEFAULT, 5).
-define(ATTRIBUTE_DOUBLE_HIT_CHANCE_COST,    1).
-define
(
   ATTRIBUTE_DOUBLE_HIT_CHANCE_MAX_POINTS,
   (
      (?ATTRIBUTE_DOUBLE_HIT_CHANCE_MAX - ?ATTRIBUTE_DOUBLE_HIT_CHANCE_MIN)
      * ?ATTRIBUTE_DOUBLE_HIT_CHANCE_COST
   )
).

-define(ATTRIBUTE_CRITICAL_HIT_CHANCE_MIN,      0).
-define(ATTRIBUTE_CRITICAL_HIT_CHANCE_MAX,      100).
-define(ATTRIBUTE_CRITICAL_HIT_CHANCE_DEFAULT,  10).
-define(ATTRIBUTE_CRITICAL_HIT_CHANCE_COST,     1).
-define
(
   ATTRIBUTE_CRITICAL_HIT_CHANCE_MAX_POINTS,
   (
      (?ATTRIBUTE_CRITICAL_HIT_CHANCE_MAX - ?ATTRIBUTE_CRITICAL_HIT_CHANCE_MIN)
      * ?ATTRIBUTE_CRITICAL_HIT_CHANCE_COST
   )
).

-define(ATTRIBUTE_DEFENSE_SCORE_MIN,      0).
-define(ATTRIBUTE_DEFENSE_SCORE_MAX,      300).
-define(ATTRIBUTE_DEFENSE_SCORE_DEFAULT,  50).
-define(ATTRIBUTE_DEFENSE_SCORE_COST,     1).
-define
(
   ATTRIBUTE_DEFENSE_SCORE_MAX_POINTS,
   (
      (?ATTRIBUTE_DEFENSE_SCORE_MAX - ?ATTRIBUTE_DEFENSE_SCORE_MIN)
      * ?ATTRIBUTE_DEFENSE_SCORE_COST
   )
).

-define(ATTRIBUTE_ATTACK_SCORE_MIN,       0).
-define(ATTRIBUTE_ATTACK_SCORE_MAX,       300).
-define(ATTRIBUTE_ATTACK_SCORE_DEFAULT,   50).
-define(ATTRIBUTE_ATTACK_SCORE_COST,      1).
-define
(
   ATTRIBUTE_ATTACK_SCORE_MAX_POINTS,
   (
      (?ATTRIBUTE_ATTACK_SCORE_MAX - ?ATTRIBUTE_ATTACK_SCORE_MIN)
      * ?ATTRIBUTE_ATTACK_SCORE_COST
   )
).

