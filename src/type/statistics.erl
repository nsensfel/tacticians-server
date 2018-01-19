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
      movement_points =
         trunc(math:ceil(math:pow(attributes:get_speed(Att), 1.8) / 20)),
      health =
         trunc(math:ceil(math:pow(attributes:get_constitution(Att), 1.8) / 20)),
      dodges =
         min(75, max(5, trunc(math:ceil(math:pow(4,
            (
               lists:sum
               (
                  [
                     attributes:get_dexterity(Att),
                     attributes:get_mind(Att),
                     attributes:get_speed(Att)
                  ]
               )
               / 3
            )))))),
      parries =
         min(75, trunc(math:ceil(math:pow(4,
            (
               lists:sum
               (
                  [
                     attributes:get_dexterity(Att),
                     attributes:get_speed(Att),
                     attributes:get_strength(Att)
                  ]
               )
               / 3
            ))))),
      damage_min = 0,
      damage_max = 100,
      accuracy = min(75, max(5, trunc(math:ceil(30 * math:log((x + 5) / 4))))),
      double_hits =
         min(100, trunc(math:ceil(
            math:pow(attributes:get_speed(Att), 2.5)
            / 1000
         ))),
      critical_hits =
         min(100, trunc(math:ceil(
            math:pow(attributes:get_intelligence(Att), 2.5)
            / 1000
        )))
   }.
