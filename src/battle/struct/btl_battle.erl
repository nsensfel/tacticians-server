-module(btl_battle).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-type id() :: ataxia_id:type().

-record
(
   battle,
   {
      related_inventory :: shr_inventory:type(),
      related_tile_ids :: ordsets:ordset(shr_tile:id()),
      map :: shr_map:type(),
      characters ::
         orddict:orddict(non_neg_integer(), btl_character:unresolved()),
      players :: orddict:orddict(non_neg_integer(), btl_player:type()),
      current_player_turn :: btl_player_turn:type()
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
      get_related_inventory/1,
      get_related_tile_ids/1,
      get_map/1,
      get_characters/1,
      get_character/2,
      get_players/1,
      get_player/2,
      get_current_player_turn/1,
      get_encoded_last_turns_effects/1,

      set_map/2,
      set_related_inventory/2,
      set_characters/2,
      set_character/3,
      set_players/2,
      set_player/3,
      set_current_player_turn/2,

      get_characters_field/0,
      get_players_field/0,
      get_related_inventory_field/0,
      get_related_tile_ids_field/0,
      get_current_player_turn_field/0
   ]
).

-export
(
   [
      new/1
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_all_timelines (Result, CurrentIndex, EndPoint, ArraySize, Players) ->
   Player = orddict:fetch(CurrentIndex, Players),
   Timeline = btl_player:get_timeline(Player),
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
-spec get_related_inventory (type()) -> shr_inventory:type().
get_related_inventory (Battle) -> Battle#battle.related_inventory.

-spec get_related_tile_ids (type()) -> ordsets:ordset(shr_tile:id()).
get_related_tile_ids (Battle) -> Battle#battle.related_tile_ids.

-spec get_map (type()) -> shr_map:type().
get_map (Battle) -> Battle#battle.map.

-spec get_characters
   (
      type()
   )
   -> orddict:orddict(non_neg_integer(), btl_character:unresolved()).
get_characters (Battle) -> Battle#battle.characters.

-spec get_character (non_neg_integer(), type()) -> btl_character:unresolved().
get_character (IX, Battle) ->
   orddict:fetch(IX, Battle#battle.characters).

-spec get_players
   (
      type()
   )
   -> orddict:orddict(non_neg_integer(), btl_player:type()).
get_players (Battle) ->
   Battle#battle.players.

-spec get_player (non_neg_integer(), type()) -> btl_player:type().
get_player (IX, Battle) ->
   orddict:fetch(IX, Battle#battle.players).

-spec get_current_player_turn (type()) -> btl_player_turn:type().
get_current_player_turn (Battle) ->
   Battle#battle.current_player_turn.

-spec get_encoded_last_turns_effects (type()) -> list(any()).
get_encoded_last_turns_effects (Battle) ->
   CurrentPlayerTurn = Battle#battle.current_player_turn,
   Players = Battle#battle.players,
   CurrentPlayerIX = btl_player_turn:get_player_ix(CurrentPlayerTurn),

   PlayersCount = orddict:size(Players),
   StartingPoint = ((CurrentPlayerIX + 1) rem PlayersCount),
   get_all_timelines([], StartingPoint, CurrentPlayerIX, PlayersCount, Players).

-spec set_map (shr_map:type(), type()) -> type().
set_map (Map, Battle) ->
   Battle#battle
   {
      map = Map,
      related_tile_ids = shr_map:get_related_tile_ids(Map)
   }.

-spec set_characters
   (
      orddict:orddict(non_neg_integer(), btl_character:unresolved()),
      type()
   )
   -> type().
set_characters (Characters, Battle) ->
   Battle#battle
   {
      characters = Characters
   }.

-spec set_character
   (
      non_neg_integer(),
      btl_character:unresolved(),
      type()
   )
   -> type().
set_character (IX, Character, Battle) ->
   Battle#battle
   {
      characters = orddict:store(IX, Character, Battle#battle.characters)
   }.

-spec set_players
   (
      orddict:orddict(non_neg_integer(), btl_player:type()),
      type()
   )
   -> type().
set_players (Players, Battle) ->
   Battle#battle
   {
      players = Players
   }.

-spec set_related_inventory ( shr_inventory:type(), type()) -> type().
set_related_inventory (Inv, Battle) ->
   Battle#battle
   {
      related_inventory = Inv
   }.

-spec set_player (non_neg_integer(), btl_player:type(), type()) -> type().
set_player (IX, Player, Battle) ->
   Battle#battle
   {
      players = orddict:store(IX, Player, Battle#battle.players)
   }.

-spec set_current_player_turn (btl_player_turn:type(), type()) -> type().
set_current_player_turn (PlayerTurn, Battle) ->
   Battle#battle
   {
      current_player_turn = PlayerTurn
   }.

-spec new (shr_map:type()) -> type().
new (Map) ->
   EmptyDict = orddict:new(),

   #battle
   {
      related_inventory = shr_inventory:default(),
      related_tile_ids = shr_map:get_related_tile_ids(Map),
      map = Map,
      characters = EmptyDict,
      players = EmptyDict,
      current_player_turn = btl_player_turn:new(0, 0)
   }.

-spec get_characters_field () -> non_neg_integer().
get_characters_field () -> #battle.characters.

-spec get_related_inventory_field () -> non_neg_integer().
get_related_inventory_field () -> #battle.related_inventory.

-spec get_related_tile_ids_field () -> non_neg_integer().
get_related_tile_ids_field () -> #battle.related_tile_ids.

-spec get_players_field () -> non_neg_integer().
get_players_field () -> #battle.players.

-spec get_current_player_turn_field () -> non_neg_integer().
get_current_player_turn_field () -> #battle.current_player_turn.
