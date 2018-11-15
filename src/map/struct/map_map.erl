-module(map_map).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-type id() :: binary().

-record
(
   map,
   {
      owner :: binary(),
      width :: integer(),
      height :: integer(),
      tile_instances :: array:array(shr_tile:instance())
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
      get_owner/1,
      get_width/1,
      get_height/1,
      get_tile_instances/1,
      get_tile_instance/2
   ]
).

-export
(
   [
      get_width_field/0,
      get_height_field/0,
      get_tile_instances_field/0
   ]
).

-export
(
   [
      from_list/4,
      update_from_list/4,
      default/1
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
-spec get_owner (type()) -> binary().
get_owner (Map) -> Map#map.owner.

-spec get_width (type()) -> integer().
get_width (Map) -> Map#map.width.

-spec get_height (type()) -> integer().
get_height (Map) -> Map#map.height.

-spec get_tile_instances (type()) -> array:array(shr_tile:instance()).
get_tile_instances (Map) -> Map#map.tile_instances.

-spec get_tile_instance (map_location:type(), type()) -> shr_tile:instance().
get_tile_instance (Location, Map) ->
   TileIX = location_to_array_index(Map#map.width, Location),
   array:get(TileIX, Map#map.tile_instances).

-spec get_width_field () -> non_neg_integer().
get_width_field () -> #map.width.

-spec get_height_field () -> non_neg_integer().
get_height_field () -> #map.height.

-spec get_tile_instances_field () -> non_neg_integer().
get_tile_instances_field () -> #map.tile_instances.

-spec from_list
   (
      binary(),
      non_neg_integer(),
      non_neg_integer(),
      list(list(binary()))
   )
   -> type().
from_list (Owner, Width, Height, List) ->
   TileInstances = lists:map(fun shr_tile:instance_from_binary_list/1, List),

   #map
   {
      owner = Owner,
      width = Width,
      height = Height,
      tile_instances = array:from_list(TileInstances)
   }.

-spec update_from_list
   (
      type(),
      non_neg_integer(),
      non_neg_integer(),
      list(list(binary()))
   )
   -> type().
update_from_list (Map, Width, Height, List) ->
   TileInstances = lists:map(fun shr_tile:instance_from_binary_list/1, List),

   Map#map
   {
      width = Width,
      height = Height,
      tile_instances = array:from_list(TileInstances)
   }.

-spec default (binary()) -> type().
default (Owner) ->
   DefaultTileInstance = shr_tile:default_tile_instance(),

   #map
   {
      owner = Owner,
      width = 32,
      height = 32,
      tile_instances =
         array:from_list(lists:duplicate(1024, DefaultTileInstance))
   }.
