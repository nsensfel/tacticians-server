-module(bm_shim).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([generate_random_battle/0 ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec generate_random_characters
   (
      non_neg_integer(),
      non_neg_integer(),
      non_neg_integer(),
      non_neg_integer(),
      bm_battlemap:type(),
      list(bm_location:type()),
      list(bm_character:type())
   )
   -> list(bm_character:type()).
generate_random_characters
(
   0,
   0,
   _CharactersPerPlayer,
   _TotalCharacterCount,
   _Battlemap,
   _ForbiddenLocations,
   Result
) ->
   Result;
generate_random_characters
(
   MaxPlayerID,
   0,
   CharactersPerPlayer,
   TotalCharacterCount,
   Battlemap,
   ForbiddenLocations,
   Result
) ->
   generate_random_characters
   (
      (MaxPlayerID - 1),
      CharactersPerPlayer,
      CharactersPerPlayer,
      TotalCharacterCount,
      Battlemap,
      ForbiddenLocations,
      Result
   );
generate_random_characters
(
   MaxPlayerID,
   PlayerCharacterCount,
   CharactersPerPlayer,
   TotalCharacterCount,
   Battlemap,
   ForbiddenLocations,
   Result
) ->
   NewCharacter =
      bm_character:random
      (
         TotalCharacterCount,
         list_to_binary(integer_to_list(MaxPlayerID)),
         bm_battlemap:get_width(Battlemap),
         bm_battlemap:get_height(Battlemap),
         ForbiddenLocations
      ),
   Character =
      case MaxPlayerID of
         0 -> bm_character:set_is_active(true, NewCharacter);
         _ -> NewCharacter
      end,

   generate_random_characters
   (
      MaxPlayerID,
      (PlayerCharacterCount - 1),
      CharactersPerPlayer,
      (TotalCharacterCount + 1),
      Battlemap,
      [bm_character:get_location(Character)|ForbiddenLocations],
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
-spec generate_random_battle () -> bm_battle:type().
generate_random_battle () ->
   %BattlemapWidth = 32, % sh_roll:between(16, 32),
   %BattlemapHeight = 32, %sh_roll:between(16, 32),
   %Battlemap = bm_battlemap:random(0, BattlemapWidth, BattlemapHeight),
   Battlemap = bm_battlemap:from_list(0, 32, 32, demo_map()),
   Characters = generate_random_characters(1, 8, 8, 0, Battlemap, [], []),
   PlayersAsList = [bm_player:new(<<"0">>), bm_player:new(<<"1">>)],

   {UsedWeaponIDs, UsedArmorIDs} =
      lists:foldl
      (
         fun (Character, {UWIDs, UAIDs}) ->
            {MWpID, SWpID} = bm_character:get_weapon_ids(Character),
            AID = bm_character:get_armor_id(Character),
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
               bm_tile:class_id_to_type_id(TileClassID),
               CurrentTileIDs
            )
         end,
         sets:new(),
         bm_battlemap:get_tile_class_ids(Battlemap)
      ),

   Battle =
      bm_battle:new
      (
         <<"0">>,
         PlayersAsList,
         Battlemap,
         Characters,
         sets:to_list(UsedWeaponIDs),
         sets:to_list(UsedArmorIDs),
         sets:to_list(UsedTileIDs)
      ),

   Battle.
