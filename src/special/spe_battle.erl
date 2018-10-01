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
-spec reserve_id () -> binary().
reserve_id () -> <<"0">>.

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

-spec get_tile_ids (array:array(shr_tile:type())) -> sets:set(binary()).
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

-spec handle_rosters
   (
      list(rst_roster:type())
   )
   -> {list(btl_character:type()), list(btl_player:type())}.
handle_rosters (_Rosters) ->
   %% TODO Unimplemented.
   {[], []}.

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

-spec commit (btl_battle:type()) -> ok.
commit (_Battle) ->
   %% TODO Unimplemented.
   ok.

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
