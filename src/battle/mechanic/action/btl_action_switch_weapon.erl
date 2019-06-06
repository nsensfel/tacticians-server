-module(btl_action_switch_weapon).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export
(
   [
      handle/3
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
      btl_action:type(),
      btl_character:type(),
      btl_character_turn_update:type()
   )
   -> {'ok', btl_character_turn_update:type()}.
handle (Action, Character, S0Update) ->
   CharacterIX = btl_action:get_actor_index(Action),

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

   {UpdatedBattle, BattleAtaxiaUpdate} =
      btl_battle:ataxia_set_character
      (
         CharacterIX,
         UpdatedCharacter,
         CharacterAtaxiaUpdate,
         btl_character_turn_update:get_battle(S0Update)
      ),

   TimelineItem = btl_turn_result:new_character_switched_weapons(CharacterIX),

   S1Update = btl_character_turn_update:add_to_timeline(TimelineItem, S0Update),
   S2Update =
      btl_character_turn_update:ataxia_set_battle
      (
         UpdatedBattle,
         BattleAtaxiaUpdate,
         S1Update
      ),

   {ok, S2Update}.
