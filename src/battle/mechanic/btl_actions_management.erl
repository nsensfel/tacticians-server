-module(btl_actions_management).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export
(
   [
      handle/2
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec perform_action
   (
      btl_action:type(),
      btl_character:type(),
      btl_character_turn_update:type()
   )
   ->
   (
      {ok, btl_character_turn_update:type()}
      | {events, list(btl_action:type()), btl_character_turn_update:type()}
   ).
perform_action (Action, Character, Update) ->
   case btl_action:get_category(Action) of
      move -> btl_action_move:handle(Action, Character, Update);
      attack -> btl_action_attack:handle(Action, Character, Update);
      switch_weapon ->
         btl_action_switch_weapon:handle(Action, Character, Update)
   end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec handle
   (
      list(btl_action:type()),
      btl_character_turn_update:type()
   )
   -> btl_character_turn_update:type().
handle ([], Update) -> Update;
handle ([BattleAction|FutureBattleActions], S0Update) ->
   case btl_action:get_actor_index(BattleAction) of
      -1 -> handle(FutureBattleActions, S0Update);
      CharacterIX ->
         S0Battle = btl_character_turn_update:get_battle(S0Update),
         {Character, S1Battle} =
            btl_battle:get_resolved_character(CharacterIX, S0Battle),

         S1Update = btl_character_turn_update:set_battle(S1Battle, S0Update),

         case btl_character:get_is_alive(Character) of
            false -> handle(FutureBattleActions, S1Update);
            true ->
               case perform_action(BattleAction, Character, S1Update) of
                  {ok, S2Update} ->
                     handle(FutureBattleActions, S2Update);
                  {events, NewEvents, S2Update} ->
                     handle
                     (
                        (NewEvents ++ FutureBattleActions),
                        S2Update
                     )
               end
         end
   end.

