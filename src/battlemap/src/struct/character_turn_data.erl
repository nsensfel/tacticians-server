-module(character_turn_data).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record
(
   type,
   {
      dirty :: boolean(),
      battle :: battle:type(),
      character :: character:type(),
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
      clean_battle/1
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec new (battle:type(), non_neg_integer()) -> type().
new (Battle, CharacterIX) ->
   Character = battle:get_character(CharacterIX, Battle),

   #type
   {
      dirty = false,
      battle = Battle,
      character = Character,
      character_ix = CharacterIX
   }.

-spec get_battle_is_dirty (type()) -> boolean().
get_battle_is_dirty (Data) -> Data#type.dirty.

-spec get_battle (type()) -> battle:type().
get_battle (Data) -> Data#type.battle.

-spec get_character (type()) -> character:type().
get_character (Data) -> Data#type.character.

-spec get_character_ix (type()) -> non_neg_integer().
get_character_ix (Data) -> Data#type.character_ix.

-spec set_battle (battle:type(), type()) -> type().
set_battle (Battle, Data) ->
   Data#type{ battle = Battle }.

-spec set_character (character:type(), type()) -> type().
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
         battle:set_character
         (
            Data#type.character_ix,
            Data#type.character,
            Data#type.battle
         )
   }.

