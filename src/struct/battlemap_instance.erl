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
      players,
      current_player_turn,
      last_player_turn
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
      get_players/1,
      get_current_player_turn/1,
      get_last_player_turn/1,

      set_battlemap/2,
      set_character_instances/2,
      set_players/2,
      set_current_player_turn/2,
      set_last_player_turn/2
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

get_players (BattlemapInstance) ->
   BattlemapInstance#battlemap_instance.players.

get_current_player_turn (BattlemapInstance) ->
   BattlemapInstance#battlemap_instance.current_player_turn.

get_last_player_turn (BattlemapInstance) ->
   BattlemapInstance#battlemap_instance.last_player_turn.

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

set_players (Players, BattlemapInstance) ->
   BattlemapInstance#battlemap_instance
   {
      players = Players
   }.

set_current_player_turn (PlayerTurn, BattlemapInstance) ->
   BattlemapInstance#battlemap_instance
   {
      current_player_turn = PlayerTurn
   }.

set_last_player_turn (PlayerTurn, BattlemapInstance) ->
   BattlemapInstance#battlemap_instance
   {
      last_player_turn = PlayerTurn
   }.
