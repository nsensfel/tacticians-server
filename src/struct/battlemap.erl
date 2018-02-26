-module(battlemap).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record
(
   battlemap,
   {
      id,
      width,
      height,
      tile_ids
   }
).

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
get_id (Battlemap) -> Battlemap#battlemap.id.
get_width (Battlemap) -> Battlemap#battlemap.width.
get_height (Battlemap) -> Battlemap#battlemap.height.
get_tile_ids (Battlemap) -> Battlemap#battlemap.tile_ids.

random (ID, Width, Height) ->
   InitialTile = tile:random_id(),
   TileIDs = generate_random_tile_ids(InitialTile, [], Width, Height, Width),

   #battlemap
   {
      id = ID,
      width = Width,
      height = Height,
      tile_ids = TileIDs
   }.
