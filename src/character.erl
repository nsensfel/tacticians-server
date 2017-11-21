-module(character).
-export
(
   [
      get_movement_points/1
   ]
).

-include("timed_cache_data.hrl").

get_movement_points (Char) -> Char#character.mov_pts.
