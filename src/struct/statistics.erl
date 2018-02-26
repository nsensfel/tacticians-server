-module(statistics).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record
(
   statistics,
   {
      movement_points,
      health,
      dodges,
      parries,
      damage_min,
      damage_max,
      accuracy,
      double_hits,
      critical_hits
   }
).

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
ceil (F) ->
   I = trunc(F),
   case (F > I) of
      true -> (I + 1);
      _ -> I
   end.

float_to_int (F) -> trunc(ceil(F)).
min_max (Min, Max, V) -> min(Max, max(Min, V)).

average ([]) -> 0;
average (L) -> lists:sum(L) / length(L).

% V | 010 | 030 | 050 | 070 | 100 |
% F | 004 | 023 | 058 | 104 | 200 |
gentle_squared_growth (V) -> float_to_int(math:pow(V, 1.8) / 20).

% V | 010 | 030 | 050 | 070 | 100 |
% F | 001 | 005 | 018 | 041 | 100 |
sudden_squared_growth (V) -> float_to_int(math:pow(V, 2.5) / 1000).

% V | 010 | 030 | 050 | 070 | 100 |
% F | 002 | 006 | 016 | 049 | 256 |
sudden_exp_growth (V) -> float_to_int(math:pow(4, V / 25)).

% V | 010 | 030 | 050 | 070 | 100 |
% F | 040 | 066 | 079 | 088 | 099 |
% Seems too generous, values for attributes below 50 should dip faster and
% lower.
already_high_slow_growth (V) -> float_to_int(30 * math:log((V + 5)/4)).

damage_base_modifier (Strength) -> ((math:pow(Strength, 1.8) / 2000.0) - 0.75).

apply_damage_base_modifier (Modifier, BaseValue) ->
   max(0, float_to_int(BaseValue + (BaseValue * Modifier))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Accessors
get_movement_points (Stats) -> Stats#statistics.movement_points.
get_health (Stats) -> Stats#statistics.health.
get_dodges (Stats) -> Stats#statistics.dodges.
get_parries (Stats) -> Stats#statistics.parries.
get_damage_min (Stats) -> Stats#statistics.damage_min.
get_damage_max (Stats) -> Stats#statistics.damage_max.
get_accuracy (Stats) -> Stats#statistics.accuracy.
get_double_hits (Stats) -> Stats#statistics.double_hits.
get_critical_hits (Stats) -> Stats#statistics.critical_hits.

get_damages (Stats) ->
   {
      Stats#statistics.damage_min,
      Stats#statistics.damage_max
   }.

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
