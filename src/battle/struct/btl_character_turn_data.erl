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
      character_current_data :: btl_character_current_data:type(),
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
      get_character_current_data/1,
      get_character_ix/1,

      set_battle/2,
      set_character/2,
      refresh_character_current_data/1
   ]
).

-export
(
   [
      clean_battle/1,
      refreshr_character/1
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec new (btl_battle:type(), non_neg_integer()) -> type().
new (Battle, CharacterIX) ->
   Character = btl_battle:get_character(CharacterIX, Battle),
   Map = btl_battle:get_map(Battle),
   CharacterCurrentData = btl_character_current_data:new(Character, Map),

   #type
   {
      dirty = false,
      battle = Battle,
      character = Character,
      character_current_data = CharacterCurrentData,
      character_ix = CharacterIX
   }.

-spec get_battle_is_dirty (type()) -> boolean().
get_battle_is_dirty (Data) -> Data#type.dirty.

-spec get_battle (type()) -> btl_battle:type().
get_battle (Data) -> Data#type.battle.

-spec get_character (type()) -> btl_character:type().
get_character (Data) -> Data#type.character.

-spec get_character_current_data (type()) -> btl_character_current_data:type().
get_character_current_data (Data) -> Data#type.character_current_data.

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

-spec refresh_character_current_data (type()) -> type().
refresh_character_current_data (Data) ->
   Battle = Data#type.battle,
   Character = Data#type.character,
   Map = btl_battle:get_map(Battle),
   CharacterCurrentData = btl_character_current_data:new(Character, Map),

   Data#type
   {
      character_current_data = CharacterCurrentData
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
            Data#type.character,
            Data#type.battle
         )
   }.

-spec refreshr_character (type()) -> type().
refreshr_character (Data) ->
   Data#type
   {
      dirty = false,
      character =
         btl_battle:get_character
         (
            Data#type.character_ix,
            Data#type.battle
         )
   }.
