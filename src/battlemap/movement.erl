-module(movement).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export
(
   [
      cross/4,
      steps_between/2
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec location_after_step
   (
      binary(),
      integer(),
      integer()
   )
   -> {integer(), integer()}.
location_after_step (Step, X, Y) ->
   case Step of
      <<"L">> -> {(X - 1), Y};
      <<"R">> -> {(X + 1), Y};
      <<"U">> -> {X, (Y - 1)};
      <<"D">> -> {X, (Y + 1)}
   end.

-spec location_to_array_index
   (
      non_neg_integer(),
      integer(),
      integer()
   )
   -> ('error' | non_neg_integer()).
location_to_array_index (ArrayWidth, X, Y) ->
   if
      (X < 0) -> error;
      (Y < 0) -> error;
      (X >= ArrayWidth) -> error;
      true -> ((Y * ArrayWidth) + X)
   end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec cross
   (
      battlemap:struct(),
      array:array(non_neg_integer(), non_neg_integer()),
      list(binary()),
      non_neg_integer(),
      non_neg_integer(),
      non_neg_integer()
   )
   -> {{non_neg_integer(), non_neg_integer()}, non_neg_integer()}.
cross (_Battlemap, _ForbiddenLocations, [], Cost, X, Y) ->
   {{X, Y}, Cost};
cross (Battlemap, ForbiddenLocations, [Step|NextSteps], Cost, X, Y) ->
   BattlemapWidth = battlemap:get_width(Battlemap),
   BattlemapTiles = battlemap:get_tile_ids(Battlemap),

   {NextX, NextY} = location_after_step(Step, X, Y),
   NextTileIX =
      location_to_array_index(BattlemapWidth, NextX, NextY),
   NextTile = array:get(NextTileIX, BattlemapTiles),
   NextCost = (Cost + tile:get_cost(NextTile)),
   IsForbidden =
      array:foldl
      (
         fun (_IX, Location, Prev) ->
            (Prev or ({NextX, NextY} == Location))
         end,
         false,
         ForbiddenLocations
      ),

   IsForbidden = false,

   cross(Battlemap, ForbiddenLocations, NextSteps, NextCost, NextX, NextY).

-spec cross
   (
      battlemap:struct(),
      array:array(non_neg_integer(), non_neg_integer()),
      list(binary()),
      {non_neg_integer(), non_neg_integer()}
   )
   -> {{non_neg_integer(), non_neg_integer()}, non_neg_integer()}.
cross (Battlemap, ForbiddenLocations, Path, {X, Y}) ->
   cross(Battlemap, ForbiddenLocations, Path, 0, X, Y).

-spec steps_between
   (
      {non_neg_integer(), non_neg_integer()},
      {non_neg_integer(), non_neg_integer()}
   )
   -> non_neg_integer().
steps_between ({OX, OY}, {DX, DY}) ->
   (abs(DY - OY) + abs(DX - OX)).
