-module(battlemap_character).
-export([encode_in_json/1]).

encode_in_json (
   {
      ID,
      Name,
      Icon,
      Portrait,
      {X, Y},
      Team,
      MovementPoints,
      AttackRange
   }
) ->
   jiffy:encode(
      {
         [
            {<<"id">>, ID},
            {<<"name">>, Name},
            {<<"icon">>, Icon},
            {<<"portrait">>, Portrait},
            {<<"loc_x">>, X},
            {<<"loc_y">>, Y},
            {<<"team">>, Team},
            {<<"mov_pts">>, MovementPoints},
            {<<"atk_rg">>, AttackRange}
         ]
      }
   ).
