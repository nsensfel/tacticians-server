-module(movement).

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
      battlemap:struct(),
      list(location:type()),
      list(direction:enum()),
      non_neg_integer(),
      location:type()
   )
   -> {location:type(), non_neg_integer()}.
cross (_Battlemap, _ForbiddenLocations, [], Cost, Location) ->
   {Location, Cost};
cross (Battlemap, ForbiddenLocations, [Step|NextSteps], Cost, Location) ->
   NextLocation = location:apply_direction(Step, Location),
   NextTile = battlemap:get_tile_id(NextLocation, Battlemap),
   NextCost = (Cost + tile:get_cost(NextTile)),
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
      battlemap:struct(),
      array:array(location:type()),
      list(direction:enum()),
      location:type()
   )
   -> {location:type(), non_neg_integer()}.
cross (Battlemap, ForbiddenLocations, Path, Location) ->
   cross(Battlemap, ForbiddenLocations, Path, 0, Location).
