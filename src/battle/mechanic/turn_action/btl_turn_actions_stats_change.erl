-module(btl_turn_actions_stats_change).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export
(
   [
      handle_max_health_changes/2
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec mod_current_health
   (
      non_neg_integer(),
      non_neg_integer(),
      btl_character_turn_update:type()
   )
   -> btl_character_turn_update:type().
mod_current_health (CurrentMaxHealth, PreviousMaxHealth, Update) ->
   Data = btl_character_turn_update:get_data(Update),
   Character = btl_character_turn_data:get_character(Data),
   CharacterIX = btl_character_turn_data:get_character_ix(Data),
   PreviousHealth = btl_character:get_current_health(Character),

   PreviousHealthRatio = (PreviousHealth / PreviousMaxHealth),
   NewHealth =
      min
      (
         CurrentMaxHealth,
         max(1, round(PreviousHealthRatio * CurrentMaxHealth))
      ),

   UpdatedCharacter = btl_character:set_current_health(NewHealth, Character),
   UpdatedData = btl_character_turn_data:set_character(UpdatedCharacter, Data),
   S0Update = btl_character_turn_update:set_data(UpdatedData, Update),

   DBQuery =
      ataxic:update_field
      (
         btl_battle:get_characters_field(),
         ataxic_sugar:update_orddict_element
         (
            CharacterIX,
            ataxic:update_field
            (
               btl_character:get_current_health_field(),
               ataxic:constant(NewHealth)
            )
         )
      ),

   S1Update =  btl_character_turn_update:add_to_db(DBQuery, S0Update),

   S1Update.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec handle_max_health_changes
   (
      btl_character_current_data:type(),
      btl_character_turn_update:type()
   )
   -> btl_character_turn_update:type().
handle_max_health_changes (PreviousData, Update) ->
   Data = btl_character_turn_update:get_data(Update),
   CurrentData = btl_character_turn_data:get_character_current_data(Data),
   CurrentStats =  btl_character_current_data:get_statistics(CurrentData),
   PreviousStats = btl_character_current_data:get_statistics(PreviousData),

   CurrentMaxHealth = shr_statistics:get_health(CurrentStats),
   PreviousMaxHealth = shr_statistics:get_health(PreviousStats),

   case (CurrentMaxHealth == PreviousMaxHealth) of
      true -> Update;
      _ -> mod_current_health(CurrentMaxHealth, PreviousMaxHealth, Update)
   end.
