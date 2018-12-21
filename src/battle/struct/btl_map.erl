-module(btl_map).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record
(
   map,
   {
      width :: integer(),
      height :: integer(),
      tile_ids :: shr_tile:instances_tuple()
   }
).

-opaque type() :: #map{}.

-export_type([type/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Accessors
-export
(
   [
      get_width/1,
      get_height/1,
      get_tile_instances/1,
      get_tile_instance/2,
      get_used_tile_ids/1
   ]
).

-export
(
   [
      from_list/3,
      from_instances_tuple/3
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec location_to_index
   (
      non_neg_integer(),
      btl_location:type()
   )
   -> ('error' | non_neg_integer()).
location_to_index (ArrayWidth, {X, Y}) ->
   if
      (X < 0) -> error;
      (Y < 0) -> error;
      (X >= ArrayWidth) -> error;
      true -> ((Y * ArrayWidth) + X)
   end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Accessors
-spec get_width (type()) -> integer().
get_width (Map) -> Map#map.width.

-spec get_height (type()) -> integer().
get_height (Map) -> Map#map.height.

-spec get_tile_instances (type()) -> shr_tile:instances_tuple().
get_tile_instances (Map) -> Map#map.tile_ids.

-spec get_tile_instance (btl_location:type(), type()) -> shr_tile:instance().
get_tile_instance (Location, Map) ->
   TileIX = location_to_index(Map#map.width, Location),
   element((TileIX + 1), Map#map.tile_ids).

-spec from_list
   (
      non_neg_integer(),
      non_neg_integer(),
      list(list(binary()))
   )
   -> type().
from_list (Width, Height, List) ->
   TileInstances = lists:map(fun shr_tile:instance_from_binary_list/1, List),

   #map
   {
      width = Width,
      height = Height,
      tile_ids = list_to_tuple(TileInstances)
   }.

-spec from_instances_tuple
   (
      non_neg_integer(),
      non_neg_integer(),
      shr_tile:instances_tuple()
   )
   -> type().
from_instances_tuple (Width, Height, TileInstances) ->
   #map
   {
      width = Width,
      height = Height,
      tile_ids = TileInstances
   }.

-spec get_used_tile_ids (type()) -> ordsets:ordset(shr_tile:class_id()).
get_used_tile_ids (Map) ->
   UsedTileIDs =
      lists:foldl
      (
         fun (TileInstance, CurrentTileIDs) ->
            ordsets:add_element
            (
               shr_tile:extract_main_class_id(TileInstance),
               CurrentTileIDs
            )
         end,
         ordsets:new(),
         tuple_to_list(Map#map.tile_ids)
      ),

   UsedTileIDs.
