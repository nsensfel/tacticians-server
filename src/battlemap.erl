-module(battlemap).
-export
(
   [
      cross/4
   ]
).

-include("timed_cache_data.hrl").

next_loc (X, Y, <<"L">>) -> {(X - 1), Y};
next_loc (X, Y, <<"R">>) -> {(X + 1), Y};
next_loc (X, Y, <<"U">>) -> {X, (Y - 1)};
next_loc (X, Y, <<"D">>) -> {X, (Y + 1)}.

loc_to_index(X, Y, Map) ->
   if
      (X < 0) -> error;
      (Y < 0) -> error;
      (X >= Map#battlemap.width) -> error;
      true -> ((Y * Map#battlemap.width) + X)
   end.

calc_new_loc (X, Y, [], Points, _Map, _OtherCharsLocs) ->
   true = (Points >= 0),
   {X, Y};
calc_new_loc (X, Y, [Step|Path], Points, Map, OtherCharsLocs) ->
   {NX, NY} = next_loc(X, Y, Step),
   NPoints =
      (
         Points
         -
         tile:get_cost
         (
            array:get
            (
               loc_to_index(X, Y, Map),
               Map#battlemap.content
            )
         )
      ),
   false = lists:member({NX, NY}, OtherCharsLocs),
   calc_new_loc(NX, NY, Path, NPoints, Map, OtherCharsLocs).

cross (Battlemap, CharInst, Path, OtherChars) ->
   {X, Y} = character_instance:get_location(CharInst),
   OtherCharsLocs =
      lists:map
      (
         fun character_instance:get_location/1,
         OtherChars
      ),
   {ok, calc_new_loc(X, Y, Path, 99, Battlemap, OtherCharsLocs)}.
