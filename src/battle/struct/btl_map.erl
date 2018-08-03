-module(btl_map).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-type id() :: binary().

-record
(
   map,
   {
      id :: id(),
      width :: integer(),
      height :: integer(),
      tile_ids :: array:array(btl_tile:instance())
   }
).

-opaque type() :: #map{}.

-export_type([type/0, id/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Accessors
-export
(
   [
      get_id/1,
      get_width/1,
      get_height/1,
      get_tile_instances/1,
      get_tile_instance/2
   ]
).

-export
(
   [
      from_list/4
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec location_to_array_index
   (
      non_neg_integer(),
      btl_location:type()
   )
   -> ('error' | non_neg_integer()).
location_to_array_index (ArrayWidth, {X, Y}) ->
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
-spec get_id (type()) -> id().
get_id (Map) -> Map#map.id.

-spec get_width (type()) -> integer().
get_width (Map) -> Map#map.width.

-spec get_height (type()) -> integer().
get_height (Map) -> Map#map.height.

-spec get_tile_instances (type()) -> array:array(btl_tile:instance()).
get_tile_instances (Map) -> Map#map.tile_ids.

-spec get_tile_instance (btl_location:type(), type()) -> btl_tile:instance().
get_tile_instance (Location, Map) ->
   TileIX = location_to_array_index(Map#map.width, Location),
   array:get(TileIX, Map#map.tile_ids).

-spec from_list
   (
      non_neg_integer(),
      non_neg_integer(),
      non_neg_integer(),
      list(list(non_neg_integer()))
   )
   -> type().
from_list (ID, Width, Height, List) ->
   TileInstances = lists:map(fun btl_tile:instance_from_ints/1, List),

   #map
   {
      id = list_to_binary(integer_to_list(ID)),
      width = Width,
      height = Height,
      tile_ids = array:from_list(TileInstances)
   }.
