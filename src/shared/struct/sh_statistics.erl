-module(sh_statistics).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record
(
   statistics,
   {
      movement_points :: non_neg_integer(),
      health :: non_neg_integer(),
      dodges :: non_neg_integer(),
      parries :: non_neg_integer(),
      damage_min :: non_neg_integer(),
      damage_max :: non_neg_integer(),
      accuracy :: non_neg_integer(),
      double_hits :: non_neg_integer(),
      critical_hits :: non_neg_integer()
   }
).

-opaque type() :: #statistics{}.

-export_type([type/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Accessors
-export
(
   [
      get_movement_points/1,
      get_health/1,
      get_dodges/1,
      get_parries/1,
      get_damage_min/1,
      get_damage_max/1,
      get_accuracy/1,
      get_double_hits/1,
      get_critical_hits/1,

      get_damages/1
   ]
).

-export
(
   [
      new/3
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec float_to_int (float()) -> integer().
float_to_int (F) ->
   I = trunc(F),
   case (F > I) of
      true -> (I + 1);
      _ -> I
   end.

-spec min_max (number(), number(), number()) -> number().
min_max (Min, Max, V) -> min(Max, max(Min, V)).

-spec average (list(number())) -> number().
%average ([]) -> 0;
average (L) -> lists:sum(L) / length(L).

% V | 010 | 030 | 050 | 070 | 100 |
% F | 004 | 023 | 058 | 104 | 200 |
-spec gentle_squared_growth (number()) -> non_neg_integer().
gentle_squared_growth (V) -> float_to_int(math:pow(V, 1.8) / 20.0).

% V | 010 | 030 | 050 | 070 | 100 |
% F | 001 | 005 | 018 | 041 | 100 |
-spec sudden_squared_growth (number()) -> non_neg_integer().
sudden_squared_growth (V) -> float_to_int(math:pow(V, 2.5) / 1000.0).

% V | 010 | 030 | 050 | 070 | 100 |
% F | 002 | 006 | 016 | 049 | 256 |
-spec sudden_exp_growth (number()) -> non_neg_integer().
sudden_exp_growth (V) -> float_to_int(math:pow(4.0, V / 25.0)).

% V | 010 | 030 | 050 | 070 | 100 |
% F | 040 | 066 | 079 | 088 | 099 |
% Seems too generous, values for attributes below 50 should dip faster and
% lower.
%-spec already_high_slow_growth (non_neg_integer()) -> non_neg_integer().
%already_high_slow_growth (V) -> float_to_int(30 * math:log((V + 5)/4)).

-spec damage_base_modifier (non_neg_integer()) -> float().
damage_base_modifier (Strength) -> ((math:pow(Strength, 1.8) / 2000.0) - 0.75).

-spec apply_damage_base_modifier
   (
      float(),
      non_neg_integer()
   )
   -> non_neg_integer().
apply_damage_base_modifier (Modifier, BaseValue) ->
   max(0, float_to_int(BaseValue + (BaseValue * Modifier))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Accessors
-spec get_movement_points (type()) -> non_neg_integer().
get_movement_points (Stats) -> Stats#statistics.movement_points.

-spec get_health (type()) -> non_neg_integer().
get_health (Stats) -> Stats#statistics.health.

-spec get_dodges (type()) -> non_neg_integer().
get_dodges (Stats) -> Stats#statistics.dodges.

-spec get_parries (type()) -> non_neg_integer().
get_parries (Stats) -> Stats#statistics.parries.

-spec get_damage_min (type()) -> non_neg_integer().
get_damage_min (Stats) -> Stats#statistics.damage_min.

-spec get_damage_max (type()) -> non_neg_integer().
get_damage_max (Stats) -> Stats#statistics.damage_max.

-spec get_accuracy (type()) -> non_neg_integer().
get_accuracy (Stats) -> Stats#statistics.accuracy.

-spec get_double_hits (type()) -> non_neg_integer().
get_double_hits (Stats) -> Stats#statistics.double_hits.

-spec get_critical_hits (type()) -> non_neg_integer().
get_critical_hits (Stats) -> Stats#statistics.critical_hits.

-spec get_damages (type()) -> {non_neg_integer(), non_neg_integer()}.
get_damages (Stats) ->
   {
      Stats#statistics.damage_min,
      Stats#statistics.damage_max
   }.

-spec new
   (
      sh_attributes:type(),
      {sh_weapon:id(), sh_weapon:id()},
      sh_armor:id()
   )
   -> type().
new (BaseAttributes, WeaponIDs, ArmorID) ->
   {ActiveWeaponID, _} = WeaponIDs,
   ActiveWeapon = sh_weapon:from_id(ActiveWeaponID),
   {MinDamage, MaxDamage} = sh_weapon:get_damages(ActiveWeapon),
   Armor = sh_armor:from_id(ArmorID),
   Attributes =
      sh_armor:apply_to_attributes
      (
         Armor,
         sh_weapon:apply_to_attributes(ActiveWeapon, BaseAttributes)
      ),
   Constitution = sh_attributes:get_constitution(Attributes),
   Dexterity = sh_attributes:get_dexterity(Attributes),
   Intelligence = sh_attributes:get_intelligence(Attributes),
   Mind = sh_attributes:get_mind(Attributes),
   Speed = sh_attributes:get_speed(Attributes),
   Strength = sh_attributes:get_strength(Attributes),
   DamageBaseModifier = damage_base_modifier(Strength),

   #statistics
   {
      movement_points =
         gentle_squared_growth
         (
            average([Mind, Constitution, Constitution, Speed, Speed, Speed])
         ),
      health =
         gentle_squared_growth(average([Mind, Constitution, Constitution])),
      dodges =
         min_max(0, 100, sudden_exp_growth(average([Dexterity, Mind, Speed]))),
      parries =
         min_max
         (
            0,
            75,
            sudden_exp_growth
            (
               average([Dexterity, Intelligence, Speed, Strength])
            )
         ),
      damage_min = apply_damage_base_modifier(DamageBaseModifier, MinDamage),
      damage_max = apply_damage_base_modifier(DamageBaseModifier, MaxDamage),
      accuracy = min_max(0, 100, sudden_squared_growth(Dexterity)),
      double_hits =
         min_max(0, 100, sudden_squared_growth(average([Mind, Speed]))),
      critical_hits = min_max(0, 100, sudden_squared_growth(Intelligence))
   }.
