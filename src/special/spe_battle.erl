-module(spe_battle).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([generate/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% DB ACCESS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec reserve_id () -> binary().
reserve_id () ->
   %% TODO Unimplemented.
   <<"0">>.

-spec commit (btl_battle:type()) -> ok.
commit (_Battle) ->
   %% TODO Unimplemented.
   ok.

%%%% USED IDS COLLECTION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec get_equipment_ids
   (
      list(btl_character:type())
   )
   -> {sets:set(binary()), sets:set(binary())}.
get_equipment_ids (Characters) ->
   {UsedWeaponIDs, UsedArmorIDs} =
      lists:foldl
      (
         fun (Character, {UWIDs, UAIDs}) ->
            {MWpID, SWpID} = btl_character:get_weapon_ids(Character),
            AID = btl_character:get_armor_id(Character),
            {
               sets:add_element(MWpID, sets:add_element(SWpID, UWIDs)),
               sets:add_element(AID, UAIDs)
            }
         end,
         {sets:new(), sets:new()},
         Characters
      ),

   {UsedWeaponIDs, UsedArmorIDs}.

-spec get_tile_ids (array:array(shr_tile:instance())) -> sets:set(binary()).
get_tile_ids (TileInstances) ->
   UsedTileIDs =
      array:sparse_foldl
      (
         fun (_IX, TileInstance, CurrentTileIDs) ->
            sets:add_element
            (
               shr_tile:extract_main_class_id(TileInstance),
               CurrentTileIDs
            )
         end,
         sets:new(),
         TileInstances
      ),

   UsedTileIDs.

%%%% ROSTERS HANDLING %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec find_random_location
   (
      btl_map:type(),
      list({non_neg_integer(), non_neg_integer()})
   )
   -> {{non_neg_integer(), non_neg_integer()}, shr_tile:type()}.
find_random_location (Map, ForbiddenLocations) ->
   MapWidth = btl_map:get_width(Map),
   MapHeight = btl_map:get_height(Map),

   Candidate =
      {
         shr_roll:between(0, (MapWidth - 1)),
         shr_roll:between(0, (MapHeight - 1))
      },

   IsForbidden = lists:member(Candidate, ForbiddenLocations),

   case IsForbidden of
      true -> find_random_location(Map, ForbiddenLocations);

      _ ->
         Tile =
            shr_tile:from_class_id
            (
               shr_tile:extract_main_class_id
               (
                  btl_map:get_tile_instance(Candidate, Map)
               )
            ),

         case (shr_tile:get_cost(Tile) > 200) of
            true -> find_random_location(Map, ForbiddenLocations);

            false -> {Candidate, Tile}
         end
   end.

-spec get_glyphs_omnimods (rst_character:type()) -> shr_omnimods:type().
get_glyphs_omnimods (RosterChar) ->
   GlyphBoardID = rst_character:get_glyph_board_id(RosterChar),
   GlyphIDs = rst_character:get_glyph_ids(RosterChar),
   GlyphBoard = shr_glyph_board:from_id(GlyphBoardID),
   Glyphs = array:map(fun rst_glyph:from_id/1, GlyphIDs),
   Result = shr_glyph_board:get_omnimods(Glyphs, GlyphBoard),

   Result.

-spec create_character
   (
      non_neg_integer(),
      rst_character:type(),
      btl_map:type(),
      list(btl_location:type())
   )
   -> btl_character:type().
create_character (PlayerIX, RosterChar, Map, ForbiddenLocations) ->
   {Location, Tile} = find_random_location(Map, ForbiddenLocations),
   TileOmnimods = shr_tile:get_omnimods(Tile),
   GlyphsOmnimods = get_glyphs_omnimods(RosterChar),

   Result =
      btl_character:new
      (
         PlayerIX,
         rst_character:get_name(RosterChar),
         optional, % TODO: link this to roster.
         GlyphsOmnimods,
         rst_character:get_portrait_id(RosterChar),
         rst_character:get_weapon_ids(RosterChar),
         rst_character:get_armor_id(RosterChar),
         Location,
         TileOmnimods
      ),

   Result.

-spec handle_rosters
   (
      list(rst_roster:type())
   )
   -> {list(btl_character:type()), list(btl_player:type())}.
handle_rosters (_Rosters) ->
   %% TODO Unimplemented.
   {[], []}.

%%%% BATTLE CREATION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec generate_battle
   (
      binary(),
      map_map:type(),
      list(rst_roster:type())
   )
   -> btl_battle:type().
generate_battle (ID, Map, Rosters) ->
   TileInstances = map_map:get_tile_instances(Map),
   BattleMap =
      btl_map:from_array
      (
         map_map:get_width(Map),
         map_map:get_height(Map),
         TileInstances
      ),
   {Characters, PlayersAsList} = handle_rosters(Rosters),
   {UsedWeaponIDs, UsedArmorIDs} = get_equipment_ids(Characters),
   UsedTileIDs = get_tile_ids(TileInstances),

   Battle =
      btl_battle:new
      (
         ID,
         PlayersAsList,
         BattleMap,
         Characters,
         sets:to_list(UsedWeaponIDs),
         sets:to_list(UsedArmorIDs),
         sets:to_list(UsedTileIDs)
      ),

   Battle.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec generate
   (
      map_map:type(),
      list(rst_roster:type())
   )
   -> btl_battle:type().
generate (Map, Rosters) ->
   ID = reserve_id(),
   Battle = generate_battle(ID, Map, Rosters),
   ok = commit(Battle),
   Battle.
