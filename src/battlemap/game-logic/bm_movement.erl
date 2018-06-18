-module(bm_movement).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([cross/4]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec cross
   (
      bm_battlemap:type(),
      list(bm_location:type()),
      list(bm_direction:enum()),
      non_neg_integer(),
      bm_location:type()
   )
   -> {bm_location:type(), non_neg_integer()}.
cross (_Battlemap, _ForbiddenLocations, [], Cost, Location) ->
   {Location, Cost};
cross (Battlemap, ForbiddenLocations, [Step|NextSteps], Cost, Location) ->
   NextLocation = bm_location:apply_direction(Step, Location),
   NextTileClassID = bm_battlemap:get_tile_class_id(NextLocation, Battlemap),
   NextTileID = bm_tile:class_id_to_type_id(NextTileClassID),
   NextTile = bm_tile:from_id(NextTileID),
   NextCost = (Cost + bm_tile:get_cost(NextTile)),
   IsForbidden =
      lists:foldl
      (
         fun (ForbiddenLocation, Prev) ->
            (Prev or (NextLocation == ForbiddenLocation))
         end,
         false,
         ForbiddenLocations
      ),

   IsForbidden = false,

   cross(Battlemap, ForbiddenLocations, NextSteps, NextCost, NextLocation).

-spec cross
   (
      bm_battlemap:type(),
      list(bm_location:type()),
      list(bm_direction:enum()),
      bm_location:type()
   )
   -> {bm_location:type(), non_neg_integer()}.
cross (Battlemap, ForbiddenLocations, Path, Location) ->
   cross(Battlemap, ForbiddenLocations, Path, 0, Location).
