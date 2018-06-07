-module(bm_battle).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-type id() :: binary().

-record
(
   battle,
   {
      id :: id(),
      used_armor_ids:: list(sh_armor:id()),
      used_weapon_ids :: list(sh_weapon:id()),
      battlemap :: bm_battlemap:type(),
      characters :: array:array(bm_character:type()),
      players :: array:array(bm_player:type()),
      current_player_turn :: bm_player_turn:type()
   }
).

-opaque type() :: #battle{}.

-export_type([type/0, id/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Accessors
-export
(
   [
      get_id/1,
      get_used_weapon_ids/1,
      get_used_armor_ids/1,
      get_battlemap/1,
      get_characters/1,
      get_character/2,
      get_players/1,
      get_player/2,
      get_current_player_turn/1,
      get_encoded_last_turns_effects/1,

      set_battlemap/2,
      set_characters/2,
      set_character/3,
      set_players/2,
      set_player/3,
      set_current_player_turn/2,

      get_characters_field/0,
      get_players_field/0,
      get_current_player_turn_field/0
   ]
).

-export
(
   [
      new/6
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_all_timelines (Result, CurrentIndex, EndPoint, ArraySize, Players) ->
   Player = array:get(CurrentIndex, Players),
   Timeline = bm_player:get_timeline(Player),
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
-spec get_id (type()) -> id().
get_id (Battle) -> Battle#battle.id.

-spec get_used_weapon_ids (type()) -> list(sh_weapon:id()).
get_used_weapon_ids (Battle) -> Battle#battle.used_weapon_ids.

-spec get_used_armor_ids (type()) -> list(sh_armor:id()).
get_used_armor_ids (Battle) -> Battle#battle.used_armor_ids.

-spec get_battlemap (type()) -> bm_battlemap:type().
get_battlemap (Battle) -> Battle#battle.battlemap.

-spec get_characters (type()) -> array:array(bm_character:type()).
get_characters (Battle) -> Battle#battle.characters.

-spec get_character (non_neg_integer(), type()) -> bm_character:type().
get_character (IX, Battle) ->
   array:get(IX, Battle#battle.characters).

-spec get_players (type()) -> array:array(bm_player:type()).
get_players (Battle) ->
   Battle#battle.players.

-spec get_player (non_neg_integer(), type()) -> bm_player:type().
get_player (IX, Battle) ->
   array:get(IX, Battle#battle.players).

-spec get_current_player_turn (type()) -> bm_player_turn:type().
get_current_player_turn (Battle) ->
   Battle#battle.current_player_turn.

-spec get_encoded_last_turns_effects (type()) -> list(any()).
get_encoded_last_turns_effects (Battle) ->
   CurrentPlayerTurn = Battle#battle.current_player_turn,
   Players = Battle#battle.players,
   CurrentPlayerIX = bm_player_turn:get_player_ix(CurrentPlayerTurn),

   PlayersCount = array:size(Players),
   StartingPoint = ((CurrentPlayerIX + 1) rem PlayersCount),
   get_all_timelines([], StartingPoint, CurrentPlayerIX, PlayersCount, Players).

-spec set_battlemap (bm_battlemap:type(), type()) -> type().
set_battlemap (Battlemap, Battle) ->
   Battle#battle
   {
      battlemap = Battlemap
   }.

-spec set_characters (array:array(bm_character:type()), type()) -> type().
set_characters (Characters, Battle) ->
   Battle#battle
   {
      characters = Characters
   }.

-spec set_character (non_neg_integer(), bm_character:type(), type()) -> type().
set_character (IX, Character, Battle) ->
   Battle#battle
   {
      characters =
         array:set
         (
            IX,
            Character,
            Battle#battle.characters
         )
   }.

-spec set_players (array:array(bm_player:type()), type()) -> type().
set_players (Players, Battle) ->
   Battle#battle
   {
      players = Players
   }.

-spec set_player (non_neg_integer(), bm_player:type(), type()) -> type().
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

-spec set_current_player_turn (bm_player_turn:type(), type()) -> type().
set_current_player_turn (PlayerTurn, Battle) ->
   Battle#battle
   {
      current_player_turn = PlayerTurn
   }.

-spec new
   (
      id(),
      list(bm_player:type()),
      bm_battlemap:type(),
      list(bm_character:type()),
      list(sh_weapon:id()),
      list(sh_armor:id())
   )
   -> type().
new (ID, PlayersAsList, Battlemap, CharactersAsList, UWIDs, UAIDs) ->
   #battle
   {
      id = ID,
      used_weapon_ids = UWIDs,
      used_armor_ids = UAIDs,
      battlemap = Battlemap,
      characters = array:from_list(CharactersAsList),
      players = array:from_list(PlayersAsList),
      current_player_turn = bm_player_turn:new(0, 0)
   }.


-spec get_characters_field () -> non_neg_integer().
get_characters_field () -> #battle.characters.

-spec get_players_field () -> non_neg_integer().
get_players_field () -> #battle.players.

-spec get_current_player_turn_field () -> non_neg_integer().
get_current_player_turn_field () -> #battle.current_player_turn.
