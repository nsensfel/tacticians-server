-module(btl_character_turn_update).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record
(
   type,
   {
      battle_is_outdated :: boolean(),
      character_is_outdated :: boolean(),

      battle :: btl_battle:type(),
      reversed_battle_updates :: list(ataxic:basic()),

      character :: btl_character:type(),
      reversed_character_updates :: list(ataxic:basic()),

      character_ix :: non_neg_integer(),

      timeline :: list(any())
   }
).

-opaque type() :: #type{}.

-export_type([type/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export
(
   [
      new/2,

      get_battle/1,
      get_character/1,

      get_character_ix/1,
      get_battle_update/1,
      get_timeline/1,

      set_battle/3,
      set_character/2,

      ataxia_set_battle/4,
      ataxia_set_character/3,

      add_to_timeline/2
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec resolve_character_at
   (
      non_neg_integer(),
      btl_battle:type()
   ) -> btl_character:type().
resolve_character_at (CharacterIX, Battle) ->
   CharacterRef = btl_battle:get_character(CharacterIX, Battle),
   Location = btl_character:get_location(CharacterRef),
   Map = btl_battle:get_map(Battle),

   TileInstance = shr_map:get_tile_instance(Location, Map),
   TileClassID = shr_tile_instance:get_tile_id(TileInstance),
   Tile = shr_tile:from_id(TileClassID),
   TileOmnimods = shr_tile:get_omnimods(Tile),

   Character = btl_character:resolve(TileOmnimods, CharacterRef),

   Character.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec new (btl_battle:type(), non_neg_integer()) -> type().
new (Battle, CharacterIX) ->
   #type
   {
      character_is_outdated = false,
      battle_is_outdated = false,

      battle = Battle,
      reversed_battle_updates = [],

      character = resolve_character_at(CharacterIX, Battle),
      reversed_character_updates = [],

      character_ix = CharacterIX,

      timeline = []
   }.

-spec get_battle (type()) -> {type(), btl_battle:type()}.
get_battle (Data) ->
   case Data#type.battle_is_outdated of
      false -> {Data, Data#type.battle};
      true ->
         {UpdatedBattle, BattleAtaxiaUpdate} =
            btl_battle:ataxia_set_character
            (
               Data#type.character_ix,
               btl_character:to_unresolved(Data#type.character),
               ataxic:sequence
               (
                  lists:reverse(Data#type.reversed_character_updates)
               ),
               Data#type.battle
            ),

         {
            Data#type
            {
               battle_is_outdated = false,
               battle = UpdatedBattle,
               reversed_battle_updates =
                  [
                     BattleAtaxiaUpdate
                     |Data#type.reversed_battle_updates
                  ],
               reversed_character_updates = []
            },
            UpdatedBattle
         }
   end.

-spec get_character (type()) -> {type(), btl_character:type()}.
get_character (Data) ->
   case Data#type.character_is_outdated of
      false -> {Data, Data#type.character};
      true ->
         {
            Data#type
            {
               character_is_outdated = false,
               character =
                  resolve_character_at
                  (
                     Data#type.character_ix,
                     Data#type.battle
                  ),
               reversed_character_updates = []
            }
         }
   end.

-spec get_character_ix (type()) -> non_neg_integer().
get_character_ix (Data) -> Data#type.character_ix.

-spec set_battle (btl_battle:type(), boolean(), type()) -> type().
set_battle (Battle, CouldAffectCharacter, Data) ->
   false = (Data#type.battle_is_outdated and CouldAffectCharacter),

   Data#type
   {
      character_is_outdated =
         (Data#type.character_is_outdated or CouldAffectCharacter),
      battle = Battle
   }.

-spec ataxia_set_battle
   (
      btl_battle:type(),
      boolean(),
      ataxic:basic(),
      type()
   )
   -> type().
ataxia_set_battle (Battle, CouldAffectCharacter, BattleUpdate, Data) ->
   false = (Data#type.battle_is_outdated and CouldAffectCharacter),

   Data#type
   {
      character_is_outdated =
         (Data#type.character_is_outdated or CouldAffectCharacter),
      battle = Battle,
      reversed_battle_updates = [BattleUpdate|Data#type.reversed_battle_updates]
   }.

-spec set_character (btl_character:type(), type()) -> type().
set_character (Character, Data) ->
   false = Data#type.character_is_outdated,

   Data#type
   {
      battle_is_outdated = true,
      character = Character
   }.

-spec ataxia_set_character
   (
      btl_character:type(),
      ataxic:basic(),
      type()
   )
   -> type().
ataxia_set_character (Character, CharacterUpdate, Data) ->
   false = Data#type.character_is_outdated,

   Data#type
   {
      battle_is_outdated = true,
      character = Character,
      reversed_character_updates =
         [CharacterUpdate|Data#type.reversed_character_updates]
   }.

-spec add_to_timeline (btl_turn_result:type(), type()) -> type().
add_to_timeline (Item, Data) ->
   Data#type
   {
      timeline =
         [btl_turn_result:encode(Item)|Data#type.timeline]
   }.

-spec get_timeline (type()) -> list(any()).
get_timeline (Data) -> Data#type.timeline.

-spec get_battle_update (type()) -> ataxic:basic().
get_battle_update (Data) ->
   {ActualData, _Battle} = get_battle(Data),

   ataxic:sequence(lists:reverse(ActualData#type.reversed_battle_updates)).
