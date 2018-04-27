-module(character_turn_data).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record
(
   type,
   {
      dirty :: boolean(),
      battle :: battle:struct(),
      character_instance :: character_instance:struct(),
      character_instance_ix :: non_neg_integer()
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
      get_character_instance/1,

      set_battle/2,
      set_character_instance/2
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
new (Battle, CharacterInstanceIX) ->
   CharacterInstance =
      battle:get_character_instance(CharacterInstanceIX, Battle),

   #type
   {
      dirty = false,
      battle = Battle,
      character_instance = CharacterInstance,
      character_instance_ix = CharacterInstanceIX
   }.

-spec get_battle_is_dirty (type()) -> boolean().
get_battle_is_dirty (Data) -> Data#type.dirty.

-spec get_battle (type()) -> battle:type().
get_battle (Data) -> Data#type.battle.

-spec get_character_instance (type()) -> character_instance:type().
get_character_instance (Data) -> Data#type.character_instance.

-spec set_battle (battle:type(), type()) -> type().
set_battle (Battle, Data) ->
   Data#type{ battle = Battle }.

-spec set_character_instance (character_instance:type(), type()) -> type().
set_character_instance (CharacterInstance, Data) ->
   Data#type
   {
      dirty = true,
      character_instance = CharacterInstance
   }.

-spec clean_battle (type()) -> type().
clean_battle (Data) ->
   Data#type
   {
      dirty = false,
      battle =
         battle:set_character_instance
         (
            Data#type.character_instance_ix,
            Data#type.character_instance,
            Data#type.battle
         )
   }.

