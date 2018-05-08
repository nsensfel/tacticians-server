-module(battlemap).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-type id() :: binary().

-record
(
   battlemap,
   {
      id :: id(),
      width :: integer(),
      height :: integer(),
      tile_ids :: array:array(tile:id())
   }
).

-opaque type() :: #battlemap{}.

-export_type([struct/0, id/0]).

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
      get_tile_ids/1,
      get_tile_id/2
   ]
).

-export
(
   [
      random/3
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec generate_random_tile_ids
   (
      tile:id(),
      list(tile:id()),
      non_neg_integer(),
      non_neg_integer(),
      non_neg_integer()
   )
   -> list(tile:id()).
generate_random_tile_ids (_PreviousTileID, Result, _X, 0, _Width) ->
   Result;
generate_random_tile_ids (PreviousTileID, Result, 0, Y, Width) ->
   generate_random_tile_ids(PreviousTileID, Result, Width, (Y - 1), Width);
generate_random_tile_ids (PreviousTileID, Result, X, Y, Width) ->
   NewTile =
      case roll:percentage() of
         N when (N >= 10) -> PreviousTileID;
         _ -> tile:random_id()
      end,
   generate_random_tile_ids(NewTile, [NewTile|Result], (X - 1), Y, Width).

-spec location_to_array_index
   (
      non_neg_integer(),
      location:type()
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
get_id (Battlemap) -> Battlemap#battlemap.id.

-spec get_width (type()) -> integer().
get_width (Battlemap) -> Battlemap#battlemap.width.

-spec get_height (type()) -> integer().
get_height (Battlemap) -> Battlemap#battlemap.height.

-spec get_tile_ids (type()) -> array:array(tile:id()).
get_tile_ids (Battlemap) -> Battlemap#battlemap.tile_ids.

-spec get_tile_id (location:type(), type()) -> tile:id().
get_tile_id (Location, Battlemap) ->
   TileIX = location_to_array_index(Battlemap#battlemap.width, Location),
   array:get(TileIX, Battlemap#battlemap.tile_ids).

-spec random
   (
      non_neg_integer(),
      non_neg_integer(),
      non_neg_integer()
   )
   -> type().
random (ID, Width, Height) ->
   InitialTile = tile:random_id(),
   TileIDs = generate_random_tile_ids(InitialTile, [], Width, Height, Width),

   #battlemap
   {
      id = list_to_binary(integer_to_list(ID)),
      width = Width,
      height = Height,
      tile_ids = array:from_list(TileIDs)
   }.
