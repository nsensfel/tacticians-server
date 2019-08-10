-define(ATTRIBUTE_DAMAGE_MODIFIER_MIN,       0).
-define(ATTRIBUTE_DAMAGE_MODIFIER_MAX,       300).
-define(ATTRIBUTE_DAMAGE_MODIFIER_DEFAULT,   100).
-define(ATTRIBUTE_DAMAGE_MODIFIER_COST,      1).

-define(ATTRIBUTE_MOVEMENT_POINTS_MIN,       8).
-define(ATTRIBUTE_MOVEMENT_POINTS_MAX,       200).
-define(ATTRIBUTE_MOVEMENT_POINTS_DEFAULT,   32).
-define(ATTRIBUTE_MOVEMENT_POINTS_COST,      1).

-define(ATTRIBUTE_HEALTH_MIN,       1).
-define(ATTRIBUTE_HEALTH_MAX,       500).
-define(ATTRIBUTE_HEALTH_DEFAULT,   100).
-define(ATTRIBUTE_HEALTH_COST,      1).

-define(ATTRIBUTE_DODGE_MIN,     0).
-define(ATTRIBUTE_DODGE_MAX,     175).
-define(ATTRIBUTE_DODGE_DEFAULT, 50).
-define(ATTRIBUTE_DODGE_COST,    1).

-define(ATTRIBUTE_PARRIES_MIN,     0).
-define(ATTRIBUTE_PARRIES_MAX,     100).
-define(ATTRIBUTE_PARRIES_DEFAULT, 5).
-define(ATTRIBUTE_PARRIES_COST,    1).

-define(ATTRIBUTE_ACCURACY_MIN,     0).
-define(ATTRIBUTE_ACCURACY_MAX,     100).
-define(ATTRIBUTE_ACCURACY_DEFAULT, 50).
-define(ATTRIBUTE_ACCURACY_COST,    1).

-define(ATTRIBUTE_DOUBLE_HITS_MIN,     0).
-define(ATTRIBUTE_DOUBLE_HITS_MAX,     100).
-define(ATTRIBUTE_DOUBLE_HITS_DEFAULT, 5).
-define(ATTRIBUTE_DOUBLE_HITS_COST,    1).

-define(ATTRIBUTE_CRITICAL_HITS_MIN,     0).
-define(ATTRIBUTE_CRITICAL_HITS_MAX,     100).
-define(ATTRIBUTE_CRITICAL_HITS_DEFAULT, 10).
-define(ATTRIBUTE_CRITICAL_HITS_COST,    1).

-define(ATTRIBUTE_DEFENSE_SCORE_MIN,   0).
-define(ATTRIBUTE_DEFENSE_SCORE_MAX,   300).
-define(ATTRIBUTE_DEFENSE_SCORE_BASE,  50).
-define(ATTRIBUTE_DEFENSE_SCORE_COST, 1).

-define(ATTRIBUTE_ATTACK_SCORE_MIN,   0).
-define(ATTRIBUTE_ATTACK_SCORE_MAX,   300).
-define(ATTRIBUTE_ATTACK_SCORE_BASE,  50).
-define(ATTRIBUTE_ATTACK_SCORE_COST, 1).

-define
(
   ATTRIBUTE_ARMOR_POINTS,
   (
      (?ATTRIBUTE_DAMAGE_MODIFIER_COST * ?ATTRIBUTE_DAMAGE_MODIFIER_BASE)
      + (?ATTRIBUTE_MOVEMENT_POINTS_COST * ?ATTRIBUTE_MOVEMENT_POINTS_BASE)
      + (?ATTRIBUTE_HEALTH_COST * ?ATTRIBUTE_HEALTH_BASE)
      + (?ATTRIBUTE_DODGE_COST * ?ATTRIBUTE_DODGE_BASE)
      + (?ATTRIBUTE_DEFENSE_SCORE_COST * ?ATTRIBUTE_DEFENSE_SCORE_BASE)
   )
).
