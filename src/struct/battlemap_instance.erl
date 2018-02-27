-module(battlemap_instance).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-opaque id() :: binary().

-record
(
   battlemap_instance,
   {
      id :: id(),
      battlemap :: battlemap:struct(),
      character_instances :: array:array(character_instance:struct()),
      player_ids :: array:array(player:id()),
      current_player_turn :: player_turn:struct(),
      last_turns_effects :: list(any())
   }
).

-opaque struct() :: #battlemap_instance{}.

-export_type([struct/0, id/0]).

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
-spec get_id (struct()) -> id().
get_id (BattlemapInstance) -> BattlemapInstance#battlemap_instance.id.

-spec get_battlemap (struct()) -> battlemap:struct().
get_battlemap (BattlemapInstance) ->
   BattlemapInstance#battlemap_instance.battlemap.

-spec get_character_instances (struct()) ->
   array:array(character_instance:struct()).
get_character_instances (BattlemapInstance) ->
   BattlemapInstance#battlemap_instance.character_instances.

-spec get_player_ids (struct()) -> array:array(player:id()).
get_player_ids (BattlemapInstance) ->
   BattlemapInstance#battlemap_instance.player_ids.

-spec get_current_player_turn (struct()) -> player_turn:struct().
get_current_player_turn (BattlemapInstance) ->
   BattlemapInstance#battlemap_instance.current_player_turn.

-spec get_last_turns_effects (struct()) -> list(any()).
get_last_turns_effects (BattlemapInstance) ->
   BattlemapInstance#battlemap_instance.last_turns_effects.

-spec set_battlemap (battlemap:struct(), struct()) -> struct().
set_battlemap (Battlemap, BattlemapInstance) ->
   BattlemapInstance#battlemap_instance
   {
      battlemap = Battlemap
   }.

-spec set_character_instances
   (
      array:array(character_instance:struct()),
      struct()
   )
   -> struct().
set_character_instances (CharacterInstances, BattlemapInstance) ->
   BattlemapInstance#battlemap_instance
   {
      character_instances = CharacterInstances
   }.

-spec set_player_ids
   (
      array:array(player:id()),
      struct()
   )
   -> struct().
set_player_ids (Players, BattlemapInstance) ->
   BattlemapInstance#battlemap_instance
   {
      player_ids = Players
   }.

-spec set_current_player_turn
   (
      player_turn:struct(),
      struct()
   )
   -> struct().
set_current_player_turn (PlayerTurn, BattlemapInstance) ->
   BattlemapInstance#battlemap_instance
   {
      current_player_turn = PlayerTurn
   }.

-spec set_last_turns_effects
   (
      list(any()),
      struct()
   )
   -> struct().
set_last_turns_effects (Effects, BattlemapInstance) ->
   BattlemapInstance#battlemap_instance
   {
      last_turns_effects = Effects
   }.

-spec random
   (
      id(),
      list(player:id()),
      battlemap:struct(),
      list(character:struct())
   )
   -> struct().
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
