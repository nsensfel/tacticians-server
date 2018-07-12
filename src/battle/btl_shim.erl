-module(btl_shim).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([generate_random_battle/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec generate_random_characters
   (
      non_neg_integer(),
      non_neg_integer(),
      non_neg_integer(),
      non_neg_integer(),
      btl_map:type(),
      list(btl_location:type()),
      list(btl_character:type())
   )
   -> list(btl_character:type()).
generate_random_characters
(
   0,
   0,
   _CharactersPerPlayer,
   _TotalCharacterCount,
   _Map,
   _ForbiddenLocations,
   Result
) ->
   Result;
generate_random_characters
(
   MaxPlayerIX,
   0,
   CharactersPerPlayer,
   TotalCharacterCount,
   Map,
   ForbiddenLocations,
   Result
) ->
   generate_random_characters
   (
      (MaxPlayerIX - 1),
      CharactersPerPlayer,
      CharactersPerPlayer,
      TotalCharacterCount,
      Map,
      ForbiddenLocations,
      Result
   );
generate_random_characters
(
   MaxPlayerIX,
   PlayerCharacterCount,
   CharactersPerPlayer,
   TotalCharacterCount,
   Map,
   ForbiddenLocations,
   Result
) ->
   NewCharacter =
      btl_character:random
      (
         TotalCharacterCount,
         MaxPlayerIX,
         btl_map:get_width(Map),
         btl_map:get_height(Map),
         ForbiddenLocations
      ),
   Character =
      case MaxPlayerIX of
         0 -> btl_character:set_is_active(true, NewCharacter);
         _ -> NewCharacter
      end,

   generate_random_characters
   (
      MaxPlayerIX,
      (PlayerCharacterCount - 1),
      CharactersPerPlayer,
      (TotalCharacterCount + 1),
      Map,
      [btl_character:get_location(Character)|ForbiddenLocations],
      [Character|Result]
   ).
-spec demo_map () -> list(non_neg_integer()).
demo_map () ->
   [
      2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
      2, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 1, 2,
      2, 0, 1, 1, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 1, 2,
      2, 0, 0, 0, 1, 0, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 0, 1, 1, 0, 0, 2,
      2, 1, 0, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 2, 2, 0, 0, 0, 1, 2,
      2, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 8, 6, 6, 6, 6, 9, 0, 0, 0, 0, 2, 0, 0, 1, 0, 0, 2,
      2, 1, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 1, 0, 0, 4, 3, 3, 3, 3, 5, 0, 0, 1, 0, 0, 0, 1, 1, 1, 0, 2,
      2, 0, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 4, 3, 3, 3, 3, 5, 0, 0, 0, 1, 0, 0, 1, 1, 1, 0, 2,
      2, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4, 3, 3, 3, 3, 5, 0, 0, 0, 1, 1, 1, 1, 0, 1, 0, 2,
      2, 0, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 0, 0, 4, 3, 3, 3, 3, 5, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 2,
      2, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 4, 3, 3, 3, 3, 5, 0, 0, 0, 0, 0, 2, 1, 1, 0, 1, 2,
      2, 0, 0, 0, 0, 1, 0, 2, 0, 1, 0, 1, 0, 0, 0, 4, 3, 3, 3, 3, 5, 0, 1, 0, 1, 0, 1, 0, 0, 0, 1, 2,
      2, 0, 1, 1, 1, 0, 0, 1, 1, 1, 0, 0, 1, 0, 0, 4, 3, 3, 3, 3, 5, 0, 0, 0, 1, 0, 1, 1, 0, 0, 0, 2,
      2, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 1, 0, 0,11, 7, 7, 7, 7,10, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 2,
      2, 0, 1, 1, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 2,
      2, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 8, 6, 6, 6, 6, 6, 6, 6, 6, 9, 0, 0, 0, 0, 0, 0, 1, 2,
      2, 1, 1, 1, 0, 1, 0, 1, 0, 1, 0, 0, 0, 0, 4, 3, 3, 3, 3, 3, 3, 3, 3, 5, 0, 0, 0, 0, 0, 1, 0, 2,
      2, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 4, 3, 3, 3, 3, 3, 3, 3, 3, 5, 0, 0, 1, 1, 0, 2, 0, 2,
      2, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 4, 3, 3, 3, 3, 3, 3, 3, 3, 5, 0, 1, 1, 0, 0, 0, 0, 2,
      2, 1, 1, 0, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 4, 3, 3, 3, 3, 3, 3, 3, 3, 5, 0, 0, 1, 1, 0, 1, 0, 2,
      2, 0, 0, 0, 1, 2, 2, 0, 0, 0, 0, 8, 6, 6,16,15, 7, 7, 7, 7, 7, 7, 7,10, 0, 0, 1, 2, 0, 1, 0, 2,
      2, 0, 0, 0, 1, 0, 0, 1, 0, 1, 0, 4, 3, 3, 3, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 2,
      2, 1, 0, 0, 1, 1, 0, 0, 0, 0, 8,16, 3, 3, 3, 5, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 1, 0, 2,
      2, 1, 0, 1, 1, 0, 0, 0, 1, 1, 4, 3, 3, 3, 3, 5, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 2,
      2, 1, 0, 0, 0, 0, 0, 0, 0, 0, 4, 3,15, 7, 7,10, 0, 1, 1, 0, 0, 1, 1, 0, 0, 2, 1, 0, 0, 0, 1, 2,
      2, 0, 0, 1, 0, 1, 0, 1, 0, 1, 4, 3, 5, 8, 6, 6, 9, 0, 0, 0, 0, 0, 0, 0, 2, 0, 1, 0, 0, 0, 1, 2,
      2, 1, 0, 0, 0, 0, 0, 0, 0, 2, 4, 3,17,16, 3, 3, 5, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 0, 0, 2,
      2, 0, 0, 1, 0, 0, 1, 1, 0, 1, 4, 3,15, 7, 7, 7,10, 0, 1, 0, 0, 0, 2, 0, 1, 0, 0, 0, 0, 1, 1, 2,
      2, 1, 1, 0, 0, 1, 0, 0, 0, 0, 4, 3, 5, 0, 0, 0, 0, 0, 1, 0, 2, 0, 1, 0, 1, 0, 0, 0, 1, 1, 1, 2,
      2, 1, 1, 1, 0, 1, 0, 1, 0, 1, 4, 3, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1, 1, 1, 2,
      6, 6, 6, 6, 6, 6, 6, 6, 6, 6,16, 3,17, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
      3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3
   ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec generate_random_battle () -> btl_battle:type().
generate_random_battle () ->
   %MapWidth = 32, % shr_roll:between(16, 32),
   %MapHeight = 32, %shr_roll:between(16, 32),
   %Map = btl_map:random(0, MapWidth, MapHeight),
   Map = btl_map:from_list(0, 32, 32, demo_map()),
   Characters = generate_random_characters(1, 8, 8, 0, Map, [], []),
   PlayersAsList = [btl_player:new(0, 8, <<"0">>), btl_player:new(1, 0, <<"1">>)],

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

   UsedTileIDs =
      array:sparse_foldl
      (
         fun (_IX, TileClassID, CurrentTileIDs) ->
            sets:add_element
            (
               btl_tile:class_id_to_type_id(TileClassID),
               CurrentTileIDs
            )
         end,
         sets:new(),
         btl_map:get_tile_class_ids(Map)
      ),

   Battle =
      btl_battle:new
      (
         <<"0">>,
         PlayersAsList,
         Map,
         Characters,
         sets:to_list(UsedWeaponIDs),
         sets:to_list(UsedArmorIDs),
         sets:to_list(UsedTileIDs)
      ),

   Battle.
