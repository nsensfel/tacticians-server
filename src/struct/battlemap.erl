-module(battlemap).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-opaque id() :: binary().

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

-opaque struct() :: #battlemap{}.

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
      get_tile_ids/1
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Accessors
-spec get_id (struct()) -> id().
get_id (Battlemap) -> Battlemap#battlemap.id.

-spec get_width (struct()) -> integer().
get_width (Battlemap) -> Battlemap#battlemap.width.

-spec get_height (struct()) -> integer().
get_height (Battlemap) -> Battlemap#battlemap.height.

-spec get_tile_ids (struct()) -> array:array(tile:id()).
get_tile_ids (Battlemap) -> Battlemap#battlemap.tile_ids.

-spec random (id(), non_neg_integer(), non_neg_integer()) -> struct().
random (ID, Width, Height) ->
   InitialTile = tile:random_id(),
   TileIDs = generate_random_tile_ids(InitialTile, [], Width, Height, Width),

   #battlemap
   {
      id = ID,
      width = Width,
      height = Height,
      tile_ids = array:from_list(TileIDs)
   }.
