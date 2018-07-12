-module(map_map).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-type id() :: binary().

-record
(
   map,
   {
      id :: id(),
      owner :: binary(),
      width :: integer(),
      height :: integer(),
      tile_class_ids :: array:array(map_tile:class_id())
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
      get_owner/1,
      get_width/1,
      get_height/1,
      get_tile_class_ids/1,
      get_tile_class_id/2
   ]
).

-export
(
   [
      from_list/5
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec location_to_array_index
   (
      non_neg_integer(),
      map_location:type()
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

-spec get_owner (type()) -> binary().
get_owner (Map) -> Map#map.owner.

-spec get_width (type()) -> integer().
get_width (Map) -> Map#map.width.

-spec get_height (type()) -> integer().
get_height (Map) -> Map#map.height.

-spec get_tile_class_ids (type()) -> array:array(map_tile:class_id()).
get_tile_class_ids (Map) -> Map#map.tile_class_ids.

-spec get_tile_class_id (map_location:type(), type()) -> map_tile:class_id().
get_tile_class_id (Location, Map) ->
   TileIX = location_to_array_index(Map#map.width, Location),
   array:get(TileIX, Map#map.tile_class_ids).

-spec from_list
   (
      non_neg_integer(),
      binary(),
      non_neg_integer(),
      non_neg_integer(),
      list(non_neg_integer())
   )
   -> type().
from_list (ID, Owner, Width, Height, List) ->
   TileClassIDs = lists:map(fun map_tile:class_id_from_int/1, List),

   #map
   {
      id = list_to_binary(integer_to_list(ID)),
      owner = Owner,
      width = Width,
      height = Height,
      tile_class_ids = array:from_list(TileClassIDs)
   }.
