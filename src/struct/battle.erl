-module(battle).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-opaque id() :: binary().

-record
(
   battle,
   {
      id :: id(),
      battlemap :: battlemap:struct(),
      character_instances :: array:array(character_instance:struct()),
      player_ids :: array:array(player:id()),
      current_player_turn :: player_turn:struct(),
      last_turns_effects :: list(any())
   }
).

-opaque struct() :: #battle{}.

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
      get_character_instance/2,
      get_player_ids/1,
      get_player_id/2,
      get_current_player_turn/1,
      get_last_turns_effects/1,

      set_battlemap/2,
      set_character_instances/2,
      set_character_instance/3,
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
get_id (Battle) -> Battle#battle.id.

-spec get_battlemap (struct()) -> battlemap:struct().
get_battlemap (Battle) ->
   Battle#battle.battlemap.

-spec get_character_instances (struct()) ->
   array:array(character_instance:struct()).
get_character_instances (Battle) ->
   Battle#battle.character_instances.

-spec get_character_instance (non_neg_integer(), struct()) ->
   character_instance:struct().
get_character_instance (IX, Battle) ->
   array:get(IX, Battle#battle.character_instances).

-spec get_player_ids (struct()) -> array:array(player:id()).
get_player_ids (Battle) ->
   Battle#battle.player_ids.

-spec get_player_id (non_neg_integer(), struct()) -> player:id().
get_player_id (IX, Battle) ->
   array:get(IX, Battle#battle.player_ids).

-spec get_current_player_turn (struct()) -> player_turn:struct().
get_current_player_turn (Battle) ->
   Battle#battle.current_player_turn.

-spec get_last_turns_effects (struct()) -> list(any()).
get_last_turns_effects (Battle) ->
   Battle#battle.last_turns_effects.

-spec set_battlemap (battlemap:struct(), struct()) -> struct().
set_battlemap (Battlemap, Battle) ->
   Battle#battle
   {
      battlemap = Battlemap
   }.

-spec set_character_instances
   (
      array:array(character_instance:struct()),
      struct()
   )
   -> struct().
set_character_instances (CharacterInstances, Battle) ->
   Battle#battle
   {
      character_instances = CharacterInstances
   }.

-spec set_character_instance
   (
      non_neg_integer(),
      character_instance:struct(),
      struct()
   )
   -> struct().
set_character_instance (IX, CharacterInstance, Battle) ->
   Battle#battle
   {
      character_instances =
         array:set
         (
            IX,
            CharacterInstance,
            Battle#battle.character_instances
         )
   }.

-spec set_player_ids
   (
      array:array(player:id()),
      struct()
   )
   -> struct().
set_player_ids (Players, Battle) ->
   Battle#battle
   {
      player_ids = Players
   }.

-spec set_current_player_turn
   (
      player_turn:struct(),
      struct()
   )
   -> struct().
set_current_player_turn (PlayerTurn, Battle) ->
   Battle#battle
   {
      current_player_turn = PlayerTurn
   }.

-spec set_last_turns_effects
   (
      list(any()),
      struct()
   )
   -> struct().
set_last_turns_effects (Effects, Battle) ->
   Battle#battle
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

   #battle
   {
      id = ID,
      battlemap = Battlemap,
      character_instances = array:from_list(CharacterInstancesAsList),
      player_ids = array:from_list(PlayersAsList),
      current_player_turn = player_turn:new(0, 0),
      last_turns_effects = []
   }.
