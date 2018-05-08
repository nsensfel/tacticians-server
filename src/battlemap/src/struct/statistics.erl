-module(statistics).

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

-opaque struct() :: #statistics{}.

-export_type([struct/0]).

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
      new/2
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec ceil (float()) -> integer().
ceil (F) ->
   I = trunc(F),
   case (F > I) of
      true -> (I + 1);
      _ -> I
   end.

-spec float_to_int (float()) -> integer().
float_to_int (F) -> ceil(F).

-spec min_max (number(), number(), number()) -> number().
min_max (Min, Max, V) -> min(Max, max(Min, V)).

-spec average (list(number())) -> number().
%average ([]) -> 0;
average (L) -> lists:sum(L) / length(L).

% V | 010 | 030 | 050 | 070 | 100 |
% F | 004 | 023 | 058 | 104 | 200 |
-spec gentle_squared_growth (number()) -> non_neg_integer().
gentle_squared_growth (V) -> float_to_int(math:pow(V, 1.8) / 20).

% V | 010 | 030 | 050 | 070 | 100 |
% F | 001 | 005 | 018 | 041 | 100 |
-spec sudden_squared_growth (number()) -> non_neg_integer().
sudden_squared_growth (V) -> float_to_int(math:pow(V, 2.5) / 1000).

% V | 010 | 030 | 050 | 070 | 100 |
% F | 002 | 006 | 016 | 049 | 256 |
-spec sudden_exp_growth (number()) -> non_neg_integer().
sudden_exp_growth (V) -> float_to_int(math:pow(4, V / 25)).

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
-spec get_movement_points (struct()) -> non_neg_integer().
get_movement_points (Stats) -> Stats#statistics.movement_points.

-spec get_health (struct()) -> non_neg_integer().
get_health (Stats) -> Stats#statistics.health.

-spec get_dodges (struct()) -> non_neg_integer().
get_dodges (Stats) -> Stats#statistics.dodges.

-spec get_parries (struct()) -> non_neg_integer().
get_parries (Stats) -> Stats#statistics.parries.

-spec get_damage_min (struct()) -> non_neg_integer().
get_damage_min (Stats) -> Stats#statistics.damage_min.

-spec get_damage_max (struct()) -> non_neg_integer().
get_damage_max (Stats) -> Stats#statistics.damage_max.

-spec get_accuracy (struct()) -> non_neg_integer().
get_accuracy (Stats) -> Stats#statistics.accuracy.

-spec get_double_hits (struct()) -> non_neg_integer().
get_double_hits (Stats) -> Stats#statistics.double_hits.

-spec get_critical_hits (struct()) -> non_neg_integer().
get_critical_hits (Stats) -> Stats#statistics.critical_hits.

-spec get_damages (struct()) -> {non_neg_integer(), non_neg_integer()}.
get_damages (Stats) ->
   {
      Stats#statistics.damage_min,
      Stats#statistics.damage_max
   }.

-spec new
   (
      attributes:struct(),
      {weapon:id(), weapon:id()}
   )
   -> struct().
new (BaseAttributes, WeaponIDs) ->
   {ActiveWeaponID, _} = WeaponIDs,
   ActiveWeapon = weapon:from_id(ActiveWeaponID),
   {MinDamage, MaxDamage} = weapon:get_damages(ActiveWeapon),
   Attributes = weapon:apply_to_attributes(BaseAttributes, ActiveWeapon),
   Constitution = attributes:get_constitution(Attributes),
   Dexterity = attributes:get_dexterity(Attributes),
   Intelligence = attributes:get_intelligence(Attributes),
   Mind = attributes:get_mind(Attributes),
   Speed = attributes:get_speed(Attributes),
   Strength = attributes:get_strength(Attributes),
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