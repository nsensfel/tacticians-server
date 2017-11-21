-export([cross/5]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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

calc_new_loc (X, Y, [], Points, _Map, _CharInstsLocs) ->
   io:format("~nPoints remaining: ~p ~n", [Points]),
   true = (Points >= 0),
   {X, Y};
calc_new_loc (X, Y, [Step|Path], Points, Map, CharInstsLocs) ->
   io:format("~nStep - Points remaining: ~p ~n", [Points]),
   {NX, NY} = next_loc(X, Y, Step),
   TileCost = 
      tile:get_cost
      (
         array:get
         (
            loc_to_index(NX, NY, Map),
            Map#battlemap.content
         )
      ),
   io:format("~nStep cost: ~p ~n", [TileCost]),
   NPoints =
      (
         Points
         -
         TileCost
      ),
   false = lists:member({NX, NY}, CharInstsLocs),
   calc_new_loc(NX, NY, Path, NPoints, Map, CharInstsLocs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cross (Battlemap, {X, Y}, Points, Path, CharInsts) ->
   calc_new_loc
   (
      X,
      Y,
      Path,
      Points,
      Battlemap,
      lists:map
      (
         fun character_instance:get_location/1,
         CharInsts
      )
   ).
