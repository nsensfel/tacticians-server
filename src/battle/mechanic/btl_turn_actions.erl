-module(btl_turn_actions).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export
(
   [
      apply_requested_actions/2
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% TODO: move this elsewhere
-spec finalize_character
   (
      btl_character_turn_update:type()
   )
   -> btl_character_turn_update:type().
finalize_character (Update) ->
   Data = btl_character_turn_update:get_data(Update),
   Character = btl_character_turn_data:get_character(Data),

   DisabledCharacter = btl_character:set_is_active(false, Character),
   UpdatedData = btl_character_turn_data:set_character(DisabledCharacter, Data),
   FinalizedData = btl_character_turn_data:clean_battle(UpdatedData),

   DBQuery =
      ataxic:update_field
      (
         btl_battle:get_characters_field(),
         ataxic_sugar:update_orddict_element
         (
            btl_character_turn_data:get_character_ix(Data),
            ataxic:update_field
            (
               btl_character:get_is_active_field(),
               ataxic:constant(false)
            )
         )
      ),

   S0Update = btl_character_turn_update:set_data(FinalizedData, Update),
   S1Update = btl_character_turn_update:add_to_db(DBQuery, S0Update),

   S1Update.

-spec handle
(
   btl_battle_action:type(),
   btl_character_turn_update:type()
)
-> btl_character_turn_update:type().
handle (BattleAction, Update) ->
   case btl_battle_action:get_category(BattleAction) of
      move -> btl_turn_actions_move:handle(BattleAction, Update);
      switch_weapon -> btl_turn_actions_switch_weapon:handle(Update);
      attack -> btl_turn_actions_attack:handle(BattleAction, Update)
   end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec apply_requested_actions
   (
      btl_character_turn_data:type(),
      btl_character_turn_request:type()
   )
   -> btl_character_turn_update:type().
apply_requested_actions (Data, Request) ->
   Actions = btl_character_turn_request:get_actions(Request),

   EmptyUpdate = btl_character_turn_update:new(Data),
   PostActionsUpdate = lists:foldl(fun handle/2, EmptyUpdate, Actions),

   finalize_character(PostActionsUpdate).
