-module(btl_movement).

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
      btl_map:type(),
      list(btl_location:type()),
      list(btl_direction:enum()),
      non_neg_integer(),
      btl_location:type()
   )
   -> {btl_location:type(), non_neg_integer()}.
cross (_Map, _ForbiddenLocations, [], Cost, Location) ->
   {Location, Cost};
cross (Map, ForbiddenLocations, [Step|NextSteps], Cost, Location) ->
   NextLocation = btl_location:apply_direction(Step, Location),
   NextTileClassID = btl_map:get_tile_class_id(NextLocation, Map),
   NextTileID = btl_tile:class_id_to_type_id(NextTileClassID),
   NextTile = btl_tile:from_id(NextTileID),
   NextCost = (Cost + btl_tile:get_cost(NextTile)),
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

   cross(Map, ForbiddenLocations, NextSteps, NextCost, NextLocation).

-spec cross
   (
      btl_map:type(),
      list(btl_location:type()),
      list(btl_direction:enum()),
      btl_location:type()
   )
   -> {btl_location:type(), non_neg_integer()}.
cross (Map, ForbiddenLocations, Path, Location) ->
   cross(Map, ForbiddenLocations, Path, 0, Location).