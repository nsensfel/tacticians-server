-module(btl_turn_actions_switch_weapon).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export
(
   [
      handle/1
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec handle
   (
      btl_character_turn_update:type()
   )
   -> btl_character_turn_update:type().
handle (Update) ->
   {S0Update, Character} = btl_character_turn_update:get_character(Update),
   CharacterIX = btl_character_turn_update:get_character_ix(S0Update),
   BaseCharacter = btl_character:get_base_character(Character),

   {UpdatedBaseCharacter, BaseCharacterAtaxiaUpdate} =
      shr_character:ataxia_switch_weapons(BaseCharacter),

   {UpdatedCharacter, CharacterAtaxiaUpdate} =
      btl_character:ataxia_set_base_character
      (
         UpdatedBaseCharacter,
         BaseCharacterAtaxiaUpdate,
         Character
      ),

   TimelineItem = btl_turn_result:new_character_switched_weapons(CharacterIX),

   S1Update = btl_character_turn_update:add_to_timeline(TimelineItem, S0Update),
   S2Update =
      btl_character_turn_update:ataxia_set_character
      (
         UpdatedCharacter,
         CharacterAtaxiaUpdate,
         S1Update
      ),

   S2Update.
