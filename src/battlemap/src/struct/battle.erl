-module(battle).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-type id() :: binary().

-record
(
   battle,
   {
      id :: id(),
      battlemap :: battlemap:struct(),
      character_instances :: array:array(character_instance:struct()),
      players :: array:array(player:struct()),
      current_player_turn :: player_turn:struct()
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
      get_players/1,
      get_player/2,
      get_current_player_turn/1,
      get_encoded_last_turns_effects/1,

      set_battlemap/2,
      set_character_instances/2,
      set_character_instance/3,
      set_players/2,
      set_player/3,
      set_current_player_turn/2
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
get_all_timelines (Result, CurrentIndex, EndPoint, ArraySize, Players) ->
   Player = array:get(CurrentIndex, Players),
   Timeline = player:get_timeline(Player),
   NextIndex = ((CurrentIndex + 1) rem ArraySize),
   NextResult = (Timeline ++ Result),
   case CurrentIndex of
      EndPoint ->
         NextResult;

      _ ->
         get_all_timelines(NextResult, NextIndex, EndPoint, ArraySize, Players)
   end.

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

-spec get_players (struct()) -> array:array(player:struct()).
get_players (Battle) ->
   Battle#battle.players.

-spec get_player (non_neg_integer(), struct()) -> player:struct().
get_player (IX, Battle) ->
   array:get(IX, Battle#battle.players).

-spec get_current_player_turn (struct()) -> player_turn:struct().
get_current_player_turn (Battle) ->
   Battle#battle.current_player_turn.

-spec get_encoded_last_turns_effects (struct()) -> list(any()).
get_encoded_last_turns_effects (Battle) ->
   CurrentPlayerTurn = Battle#battle.current_player_turn,
   Players = Battle#battle.players,
   CurrentPlayerIX = player_turn:get_player_ix(CurrentPlayerTurn),

   PlayersCount = array:size(Players),
   StartingPoint = ((CurrentPlayerIX + 1) rem PlayersCount),
   get_all_timelines([], StartingPoint, CurrentPlayerIX, PlayersCount, Players).

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

-spec set_players
   (
      array:array(player:struct()),
      struct()
   )
   -> struct().
set_players (Players, Battle) ->
   Battle#battle
   {
      players = Players
   }.

-spec set_player
   (
      non_neg_integer(),
      player:struct(),
      struct()
   )
   -> struct().
set_player (IX, Player, Battle) ->
   Battle#battle
   {
      players =
         array:set
         (
            IX,
            Player,
            Battle#battle.players
         )
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

-spec random
   (
      id(),
      list(player:struct()),
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
      players = array:from_list(PlayersAsList),
      current_player_turn = player_turn:new(0, 0)
   }.