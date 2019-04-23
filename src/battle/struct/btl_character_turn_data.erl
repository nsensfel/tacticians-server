-module(btl_character_turn_data).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record
(
   type,
   {
      dirty :: boolean(),
      battle :: btl_battle:type(),
      character :: btl_character:type(),
      character_ix :: non_neg_integer()
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

      get_battle_is_dirty/1,
      get_battle/1,
      get_character/1,
      get_character_ix/1,

      set_battle/2,
      set_character/2
   ]
).

-export
(
   [
      clean_battle/1,
      refresh_character/1
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec resolve_character_at
   (
      btl_battle:type(),
      non_neg_integer()
   ) -> btl_charater:type().
resolve_character_at (Battle, CharacterIX) ->
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
      dirty = false,
      battle = Battle,
      character = resolve_character_at(Battle, CharacterIX),
      character_ix = CharacterIX
   }.

-spec get_battle_is_dirty (type()) -> boolean().
get_battle_is_dirty (Data) -> Data#type.dirty.

-spec get_battle (type()) -> btl_battle:type().
get_battle (Data) -> Data#type.battle.

-spec get_character (type()) -> btl_character:type().
get_character (Data) -> Data#type.character.

-spec get_character_ix (type()) -> non_neg_integer().
get_character_ix (Data) -> Data#type.character_ix.

-spec set_battle (btl_battle:type(), type()) -> type().
set_battle (Battle, Data) ->
   Data#type{ battle = Battle }.

-spec set_character (btl_character:type(), type()) -> type().
set_character (Character, Data) ->
   Data#type
   {
      dirty = true,
      character = Character
   }.

-spec clean_battle (type()) -> type().
clean_battle (Data) ->
   Data#type
   {
      dirty = false,
      battle =
         btl_battle:set_character
         (
            Data#type.character_ix,
            btl_character:to_unresolved(Data#type.character),
            Data#type.battle
         )
   }.

-spec refresh_character (type()) -> type().
refresh_character (Data) ->
   Data#type
   {
      dirty = false,
      character = resolve_character_at(Data#type.battle, Data#type.character_ix)
   }.
