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
      characters :: orddict:orddict(non_neg_integer(), btl_character:either()),
      players :: orddict:orddict(non_neg_integer(), btl_player:type()),
      current_player_turn :: btl_player_turn:type(),
      conditions :: btl_condition:collection()
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
      get_resolved_character/2,
      get_players/1,
      get_player/2,
      get_current_player_turn/1,
      get_encoded_last_turns_effects/1,
      get_conditions/1,

      set_map/2,
      ataxia_set_map/2,
      ataxia_set_map/3,

      set_related_inventory/2,
      ataxia_set_related_inventory/2,
      ataxia_set_related_inventory/3,

      set_characters/2,
      ataxia_set_characters/2,
      ataxia_set_characters/3,

      set_character/3,
      ataxia_set_character/3,
      ataxia_set_character/4,

      add_character/2,
      ataxia_add_character/2,

      set_players/2,
      ataxia_set_players/2,
      ataxia_set_players/3,

      set_player/3,
      ataxia_set_player/3,
      ataxia_set_player/4,

      add_player/2,
      ataxia_add_player/2,

      set_current_player_turn/2,
      ataxia_set_current_player_turn/2,
      ataxia_set_current_player_turn/3,

      set_conditions/2,
      ataxia_set_conditions/2,
      ataxia_set_conditions/3,

      get_characters_field/0,
      get_players_field/0,
      get_related_inventory_field/0,
      get_related_tile_ids_field/0,
      get_current_player_turn_field/0,
      get_conditions_field/0
   ]
).

-export
(
   [
      new/1
   ]
).

-export
(
   [
     resolve_character/2
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
   -> orddict:orddict(non_neg_integer(), btl_character:either()).
get_characters (Battle) -> Battle#battle.characters.

-spec get_character (non_neg_integer(), type()) -> btl_character:either().
get_character (IX, Battle) -> orddict:fetch(IX, Battle#battle.characters).

-spec get_resolved_character
   (
      non_neg_integer(),
      type()
   )
   -> {btl_character:type(), type()}.
get_resolved_character (IX, Battle) ->
   Character = orddict:fetch(IX, Battle#battle.characters),

   case btl_character:is_unresolved(Character) of
      true ->
         ResolvedCharacter = resolve_character(Character, Battle),
         {
            ResolvedCharacter,
            Battle#battle
            {
               characters =
                  orddict:store(IX, ResolvedCharacter, Battle#battle.characters)
            }
         };

      false -> {Character, Battle}
   end.

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

-spec ataxia_set_map (shr_map:type(), type()) -> {type(), ataxic:basic()}.
ataxia_set_map (Map, Battle) ->
   ataxia_set_map(Map, ataxic:constant(Map), Battle).

-spec ataxia_set_map
   (
      shr_map:type(),
      ataxic:basic(),
      type()
   )
   -> {type(), ataxic:basic()}.
ataxia_set_map (Map, MapUpdate, Battle) ->
   OldRelatedTileIds = Battle#battle.related_tile_ids,
   NewRelatedTileIds = shr_map:get_related_tile_ids(Map),

   OrdsetUpdate =
      ataxic_sugar:update_ordset(OldRelatedTileIds, NewRelatedTileIds),

   {
      Battle#battle
      {
         map = Map,
         related_tile_ids = NewRelatedTileIds
      },
      ataxic:sequence
      (
         [
            ataxic:update_field(get_map_field(), MapUpdate),
            ataxic:update_field(get_related_tile_ids_field(), OrdsetUpdate)
         ]
      )
   }.


-spec set_characters
   (
      orddict:orddict(non_neg_integer(), btl_character:either()),
      type()
   )
   -> type().
set_characters (Characters, Battle) ->
   Battle#battle
   {
      characters = Characters
   }.

-spec ataxia_set_characters
   (
      orddict:orddict(non_neg_integer(), btl_character:either()),
      type()
   )
   -> {type(), ataxic:basic()}.
ataxia_set_characters (Characters, Battle) ->
   UnresolvedCharacters =
      orddict:map
      (
         fun (_Key, Character) ->
            btl_character:to_unresolved(Character)
         end,
         Characters
      ),

   ataxia_set_characters
   (
      Characters,
      ataxic:constant(UnresolvedCharacters),
      Battle
   ).

-spec ataxia_set_characters
   (
      orddict:orddict(non_neg_integer(), btl_character:either()),
      ataxic:basic(),
      type()
   )
   -> {type(), ataxic:basic()}.
ataxia_set_characters (Characters, CharactersUpdate, Battle) ->
   {
      set_characters(Characters, Battle),
      ataxic:update_field
      (
         get_characters_field(),
         CharactersUpdate
      )
   }.

-spec set_character
   (
      non_neg_integer(),
      btl_character:either(),
      type()
   )
   -> type().
set_character (IX, Character, Battle) ->
   Battle#battle
   {
      characters = orddict:store(IX, Character, Battle#battle.characters)
   }.

-spec add_character
   (
      btl_character:either(),
      type()
   )
   -> {non_neg_integer(), type()}.
add_character (Character, Battle) ->
   IX = orddict:size(Battle#battle.characters),
   {IX, set_character(IX, Character, Battle)}.

-spec ataxia_add_character
   (
      btl_character:either(),
      type()
   )
   -> {non_neg_integer(), type(), ataxic:basic()}.
ataxia_add_character (Character, Battle) ->
   IX = orddict:size(Battle#battle.characters),
   {S0Battle, AtaxicUpdate} = ataxia_set_character(IX, Character, Battle),
   {IX, S0Battle, AtaxicUpdate}.

-spec ataxia_set_character
   (
      non_neg_integer(),
      btl_character:either(),
      type()
   )
   -> {type(), ataxic:basic()}.
ataxia_set_character (IX, Character, Battle) ->
   ataxia_set_character
   (
      IX,
      Character,
      ataxic:constant(btl_character:to_unresolved(Character)),
      Battle
   ).

-spec ataxia_set_character
   (
      non_neg_integer(),
      btl_character:either(),
      ataxic:basic(),
      type()
   )
   -> {type(), ataxic:basic()}.
ataxia_set_character (IX, Character, CharacterUpdate, Battle) ->
   {
      set_character(IX, Character, Battle),
      ataxic:update_field
      (
         get_characters_field(),
         ataxic_sugar:update_orddict_element(IX, CharacterUpdate)
      )
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

-spec ataxia_set_players
   (
      orddict:orddict(non_neg_integer(), btl_player:type()),
      type()
   )
   -> {type(), ataxic:basic()}.
ataxia_set_players (Players, Battle) ->
   ataxia_set_players(Players, ataxic:constant(Players), Battle).

-spec ataxia_set_players
   (
      orddict:orddict(non_neg_integer(), btl_player:type()),
      ataxic:basic(),
      type()
   )
   -> {type(), ataxic:basic()}.
ataxia_set_players (Players, PlayersUpdate, Battle) ->
   {
      set_players(Players, Battle),
      ataxic:update_field
      (
         get_players_field(),
         PlayersUpdate
      )
   }.

-spec set_related_inventory (shr_inventory:type(), type()) -> type().
set_related_inventory (Inv, Battle) ->
   Battle#battle
   {
      related_inventory = Inv
   }.

-spec ataxia_set_related_inventory
   (
      shr_inventory:type(),
      type()
   )
   -> {type(), ataxic:basic()}.
ataxia_set_related_inventory (Inv, Battle) ->
   ataxia_set_related_inventory(Inv, ataxic:constant(Inv), Battle).

-spec ataxia_set_related_inventory
   (
      shr_inventory:type(),
      ataxic:basic(),
      type()
   )
   -> {type(), ataxic:basic()}.
ataxia_set_related_inventory (Inv, InvUpdate, Battle) ->
   {
      set_related_inventory(Inv, Battle),
      ataxic:update_field(get_related_inventory_field(), InvUpdate)
   }.

-spec set_player (non_neg_integer(), btl_player:type(), type()) -> type().
set_player (IX, Player, Battle) ->
   Battle#battle
   {
      players = orddict:store(IX, Player, Battle#battle.players)
   }.

-spec ataxia_set_player
   (
      non_neg_integer(),
      btl_player:type(),
      type()
   )
   -> {type(), ataxic:basic()}.
ataxia_set_player (IX, Player, Battle) ->
   ataxia_set_player(IX, Player, ataxic:constant(Player), Battle).

-spec ataxia_set_player
   (
      non_neg_integer(),
      btl_player:type(),
      ataxic:basic(),
      type()
   )
   -> {type(), ataxic:basic()}.
ataxia_set_player (IX, Player, PlayerUpdate, Battle) ->
   {
      set_player(IX, Player, Battle),
      ataxic:update_field
      (
         get_players_field(),
         ataxic_sugar:update_orddict_element(IX, PlayerUpdate)
      )
   }.

-spec add_player (btl_player:type(), type()) -> {non_neg_integer(), type()}.
add_player (Player, Battle) ->
   IX = orddict:size(Battle#battle.players),
   {IX, set_player(IX, Player, Battle)}.

-spec ataxia_add_player
   (
      btl_player:type(),
      type()
   )
   -> {non_neg_integer(), type(), ataxic:basic()}.
ataxia_add_player (Player, Battle) ->
   IX = orddict:size(Battle#battle.players),
   {S0Battle, AtaxicUpdate} = ataxia_set_player(IX, Player, Battle),
   {IX, S0Battle, AtaxicUpdate}.

-spec set_current_player_turn (btl_player_turn:type(), type()) -> type().
set_current_player_turn (PlayerTurn, Battle) ->
   Battle#battle
   {
      current_player_turn = PlayerTurn
   }.

-spec ataxia_set_current_player_turn
   (
      btl_player_turn:type(),
      type()
   )
   -> {type(), ataxic:basic()}.
ataxia_set_current_player_turn (PlayerTurn, Battle) ->
   ataxia_set_current_player_turn
   (
      PlayerTurn,
      ataxic:constant(PlayerTurn),
      Battle
   ).

-spec ataxia_set_current_player_turn
   (
      btl_player_turn:type(),
      ataxic:basic(),
      type()
   )
   -> {type(), ataxic:basic()}.
ataxia_set_current_player_turn (PlayerTurn, PlayerTurnUpdate, Battle) ->
   {
      set_current_player_turn(PlayerTurn, Battle),
      ataxic:update_field
      (
         get_current_player_turn_field(),
         PlayerTurnUpdate
      )
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

-spec resolve_character
   (
      btl_character:either(),
      type()
   )
   -> btl_character:type().
resolve_character (Character, Battle) ->
   case btl_character:is_unresolved(Character) of
      true ->
         btl_character:resolve
         (
            shr_tile:get_omnimods
            (
               shr_tile:from_id
               (
                  shr_tile_instance:get_tile_id
                  (
                     shr_map:get_tile_instance
                     (
                        btl_character:get_location(Character),
                        Battle#battle.map
                     )
                  )
               )
            ),
            Character
         );

      false -> Character
   end.

-spec get_conditions (type()) -> btl_condition:collection().
get_conditions (#battle{ conditions = R }) -> R.

-spec set_conditions (btl_condition:collection(), type()) -> type().
set_conditions (Conditions, Battle) ->
   Battle#battle{ conditions = Conditions }.

-spec ataxia_set_conditions
   (
      btl_condition:collection(),
      ataxic:basic(),
      type()
   )
   -> {type(), ataxic:basic()}.
ataxia_set_conditions (Conditions, Update, Battle) ->
   {
      set_conditions(Conditions, Battle),
      ataxic:update_field
      (
         get_conditions_field(),
         Update
      )
   }.

-spec ataxia_set_conditions
   (
      btl_condition:collection(),
      type()
   )
   -> {type(), ataxic:basic()}.
ataxia_set_conditions (Conditions, Battle) ->
   ataxia_set_conditions
   (
      Conditions,
      ataxic:constant(Conditions),
      Battle
   ).

-spec get_characters_field () -> non_neg_integer().
get_characters_field () -> #battle.characters.

-spec get_map_field () -> non_neg_integer().
get_map_field () -> #battle.map.

-spec get_related_inventory_field () -> non_neg_integer().
get_related_inventory_field () -> #battle.related_inventory.

-spec get_related_tile_ids_field () -> non_neg_integer().
get_related_tile_ids_field () -> #battle.related_tile_ids.

-spec get_players_field () -> non_neg_integer().
get_players_field () -> #battle.players.

-spec get_current_player_turn_field () -> non_neg_integer().
get_current_player_turn_field () -> #battle.current_player_turn.

-spec get_conditions_field() -> non_neg_integer().
get_conditions_field () -> #battle.conditions.
