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
      get_critical_hits/1
   ]
).

-export
(
   [
      calc_for/2
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
float_to_int (F) -> trunc(math:ceil(F)).
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

calc_for (Att, Wp) ->
   #statistics
   {
      movement_points = gentle_squared_growth(attributes:get_speed(Att)),
      health = gentle_squared_growth(attributes:get_constitution(Att)),
      dodges =
         min_max
         (
            5,
            75,
            sudden_exp_growth
            (
               average
               (
                  [
                     attributes:get_dexterity(Att),
                     attributes:get_mind(Att),
                     attributes:get_speed(Att)
                  ]
               )
            )
         ),
      parries =
         min_max
         (
            0,
            75,
            sudden_exp_growth
            (
               average
               (
                  [
                     attributes:get_dexterity(Att),
                     attributes:get_speed(Att),
                     attributes:get_strength(Att)
                  ]
               )
            )
         ),
      damage_min = 0,
      damage_max = 100,
      accuracy =
         already_high_slow_growth(attributes:get_dexterity(Att)),
      double_hits =
         min_max(0, 100, sudden_squared_growth(attributes:get_speed(Att))),
      critical_hits =
         min_max(0, 100, sudden_squared_growth(attributes:get_intelligence(Att)))
   }.
