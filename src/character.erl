-module(character).
-export
(
   [
      get_movement_points/1,
      get_attack_range/1,
      get_max_health/1
   ]
).

-include("timed_cache_data.hrl").

get_movement_points (Char) -> Char#character.mov_pts.
get_attack_range (Char) -> Char#character.atk_rg.

get_max_health (Char) -> Char#character.health.
