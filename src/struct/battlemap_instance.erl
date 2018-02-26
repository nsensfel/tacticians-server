-module(battlemap_instance).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record
(
   battlemap_instance,
   {
      id,
      battlemap,
      character_instances,
      player_ids,
      current_player_turn,
      last_turns_effects
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
      get_battlemap/1,
      get_character_instances/1,
      get_player_ids/1,
      get_current_player_turn/1,
      get_last_turns_effects/1,

      set_battlemap/2,
      set_character_instances/2,
      set_player_ids/2,
      set_current_player_turn/2,
      set_last_turns_effects/2
   ]
).

-export
(
   [
      random/4
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Accessors
get_id (BattlemapInstance) -> BattlemapInstance#battlemap_instance.id.

get_battlemap (BattlemapInstance) ->
   BattlemapInstance#battlemap_instance.battlemap.

get_character_instances (BattlemapInstance) ->
   BattlemapInstance#battlemap_instance.character_instances.

get_player_ids (BattlemapInstance) ->
   BattlemapInstance#battlemap_instance.player_ids.

get_current_player_turn (BattlemapInstance) ->
   BattlemapInstance#battlemap_instance.current_player_turn.

get_last_turns_effects (BattlemapInstance) ->
   BattlemapInstance#battlemap_instance.last_turns_effects.

set_battlemap (Battlemap, BattlemapInstance) ->
   BattlemapInstance#battlemap_instance
   {
      battlemap = Battlemap
   }.

set_character_instances (CharacterInstances, BattlemapInstance) ->
   BattlemapInstance#battlemap_instance
   {
      character_instances = CharacterInstances
   }.

set_player_ids (Players, BattlemapInstance) ->
   BattlemapInstance#battlemap_instance
   {
      player_ids = Players
   }.

set_current_player_turn (PlayerTurn, BattlemapInstance) ->
   BattlemapInstance#battlemap_instance
   {
      current_player_turn = PlayerTurn
   }.

set_last_turns_effects (Effects, BattlemapInstance) ->
   BattlemapInstance#battlemap_instance
   {
      last_turns_effects = Effects
   }.

random (ID, PlayersAsList, Battlemap, Characters) ->
   BattlemapWidth = battlemap:get_width(Battlemap),
   BattlemapHeight = battlemap:get_height(Battlemap),
   {CharacterInstancesAsList, _ForbiddenLocations} =
      lists:mapfoldl
      (
         fun (Character, ForbiddenLocations) ->
            CharacterOwner = character:get_owner_id(Character),
            NewCharacterInstance =
               character_instance:random
               (
                  Character,
                  BattlemapWidth,
                  BattlemapHeight,
                  ForbiddenLocations
               ),
            NewCharacterInstanceActive =
               case CharacterOwner of
                  <<"0">> ->
                     character_instance:set_is_active
                     (
                        true,
                        NewCharacterInstance
                     );

                  _ ->
                     NewCharacterInstance
               end,
            NewCharacterInstanceLocation =
               character_instance:get_location(NewCharacterInstanceActive),
            {
               NewCharacterInstanceActive,
               [NewCharacterInstanceLocation|ForbiddenLocations]
            }
         end,
         [],
         Characters
      ),

   #battlemap_instance
   {
      id = ID,
      battlemap = Battlemap,
      character_instances = array:from_list(CharacterInstancesAsList),
      player_ids = array:from_list(PlayersAsList),
      current_player_turn = player_turn:new(0, 0),
      last_turns_effects = []
   }.
