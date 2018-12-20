-module(spe_battle).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([generate/2, add_to/2]).

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
   -> {ordsets:ordset(binary()), ordsets:ordset(binary())}.
get_equipment_ids (Characters) ->
   {UsedWeaponIDs, UsedArmorIDs} =
      lists:foldl
      (
         fun (Character, {UWIDs, UAIDs}) ->
            {MWpID, SWpID} = btl_character:get_weapon_ids(Character),
            AID = btl_character:get_armor_id(Character),
            {
               ordsets:add_element(MWpID, ordsets:add_element(SWpID, UWIDs)),
               ordsets:add_element(AID, UAIDs)
            }
         end,
         {ordsets:new(), ordsets:new()},
         Characters
      ),

   {UsedWeaponIDs, UsedArmorIDs}.

-spec get_tile_ids
   (
      array:array(shr_tile:instance())
   )
   -> ordsets:ordset(binary()).
get_tile_ids (TileInstances) ->
   UsedTileIDs =
      array:sparse_foldl
      (
         fun (_IX, TileInstance, CurrentTileIDs) ->
            ordsets:add_element
            (
               shr_tile:extract_main_class_id(TileInstance),
               CurrentTileIDs
            )
         end,
         ordsets:new(),
         TileInstances
      ),

   UsedTileIDs.

%%%% ROSTERS HANDLING %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec find_random_location
   (
      btl_map:type(),
      ordsets:ordset({non_neg_integer(), non_neg_integer()})
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

   IsForbidden = ordsets:is_element(Candidate, ForbiddenLocations),

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
   Glyphs = lists:map(fun shr_glyph:from_id/1, GlyphIDs),
   case shr_glyph_board:get_omnimods_with_glyphs(Glyphs, GlyphBoard) of
      {ok, Result} -> Result;
      error -> shr_omnimods:new([], [], [], [])
   end.

-spec create_character
   (
      non_neg_integer(),
      rst_character:type(),
      btl_map:type(),
      ordsets:ordset(btl_location:type())
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

-spec handle_characters
   (
      list(rst_character:type()),
      non_neg_integer(),
      list(btl_character:type()),
      btl_map:type(),
      ordsets:ordset(btl_location:type())
   )
   -> list(btl_character:type()).
handle_characters ([], _PlayerIX, Characters, _Map, _UsedLocations) ->
   Characters;
handle_characters
(
   [RosterCharacter|NextRosterCharacters],
   PlayerIX,
   Characters,
   Map,
   UsedLocations
) ->
   NewCharacter =
      create_character(PlayerIX, RosterCharacter, Map, UsedLocations),

   handle_characters
   (
      NextRosterCharacters,
      PlayerIX,
      [NewCharacter|Characters],
      Map,
      [btl_character:get_location(NewCharacter)|UsedLocations]
   ).

-spec handle_roster
   (
      rst_roster:type(),
      non_neg_integer(),
      btl_map:type(),
      ordsets:ordset(btl_location:type())
   )
   -> {list(btl_character:type()), btl_player:type()}.
handle_roster
(
   Roster,
   PlayersCount,
   Map,
   UsedLocations
) ->
   NewPlayer = btl_player:new(PlayersCount, 0, rst_roster:get_owner(Roster)),
   NewCharacters =
      handle_characters
      (
         array:to_list(rst_roster:get_characters(Roster)),
         PlayersCount,
         [],
         Map,
         UsedLocations
      ),

   {NewCharacters, NewPlayer}.


%%%% BATTLE CREATION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec generate_battle
   (
      binary(),
      map_map:type(),
      rst_roster:type()
   )
   -> btl_battle:type().
generate_battle (ID, Map, Roster) ->
   TileInstances = map_map:get_tile_instances(Map),
   BattleMap =
      btl_map:from_array
      (
         map_map:get_width(Map),
         map_map:get_height(Map),
         TileInstances
      ),
   {Characters, FirstPlayer} =
      handle_roster(Roster, 0, BattleMap, ordsets:new()),

   {UsedWeaponIDs, UsedArmorIDs} = get_equipment_ids(Characters),
   UsedTileIDs = get_tile_ids(TileInstances),

   Battle =
      btl_battle:new
      (
         ID,
         [FirstPlayer],
         BattleMap,
         Characters,
         UsedWeaponIDs,
         UsedArmorIDs,
         UsedTileIDs
      ),

   Battle.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec generate
   (
      map_map:type(),
      rst_roster:type()
   )
   -> btl_battle:type().
generate (Map, Roster) ->
   ID = reserve_id(),
   Battle = generate_battle(ID, Map, Roster),
   ok = commit(Battle),
   Battle.

-spec add_to
   (
      rst_roster:type(),
      btl_battle:type()
   )
   -> btl_battle:type().
add_to (_Roster, Battle) ->
   Battle.
