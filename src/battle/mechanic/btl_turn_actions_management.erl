-module(btl_turn_actions_management).
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
-spec deactivate_character
   (
      btl_character_turn_update:type()
   )
   -> btl_character_turn_update:type().
deactivate_character (Update) ->
   {S0Update, Character} = btl_character_turn_update:get_character(Update),

   {UpdatedCharacter, CharacterAtaxiaUpdate} =
      btl_character:ataxia_set_is_active(false, Character),

   S1Update =
      btl_character_turn_update:ataxia_set_character
      (
         UpdatedCharacter,
         CharacterAtaxiaUpdate,
         S0Update
      ),

   S1Update.

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

-spec handle_actions
   (
      list(btl_action:type()),
      btl_character_turn_update:type()
   )
   -> btl_character_turn_update:type().
handle_actions ([], Update) -> Update;
handle_actions ([BattleAction|FutureBattleActions], S0Update) ->
   case btl_action:get_actor_index(BattleAction) of
      -1 -> handle_actions(FutureBattleActions, S0Update);
      CharacterIX ->
         S0Battle = btl_character_turn_update:get_battle(S0Update),
         {Character, S1Battle} =
            btl_battle:get_resolved_character(CharacterIX, S0Battle),

         S1Update = btl_character_turn_update:set_battle(S1Battle, S0Update),

         case btl_character:is_alive(Character) of
            false -> handle_actions(FutureBattleActions, S1Update);
            true ->
               case perform_action(BattleAction, Character, S1Update) of
                  {ok, S2Update} ->
                     handle_actions(FutureBattleActions, S2Update);
                  {events, NewEvents, S2Update} ->
                     handle_actions
                     (
                        (NewEvents ++ FutureBattleActions),
                        S2Update
                     )
               end
         end
   end.

-spec update_timeline
   (
      btl_character_turn_update:type()
   )
   -> btl_character_turn_update:type().
update_timeline (Update) ->
   NewTimelineElements = btl_character_turn_update:get_timeline(Update),
   {S0Update, Battle} = btl_character_turn_update:get_battle(Update),
   PlayerTurn = btl_battle:get_current_player_turn(Battle),
   PlayerIX = btl_player_turn:get_player_ix(PlayerTurn),
   Player = btl_battle:get_player(PlayerIX, Battle),

   {UpdatedPlayer, PlayerAtaxiaUpdate} =
      btl_player:ataxia_add_to_timeline(NewTimelineElements, Player),

   {UpdatedBattle, BattleAtaxiaUpdate} =
      btl_battle:ataxia_set_player
      (
         PlayerIX,
         UpdatedPlayer,
         PlayerAtaxiaUpdate,
         Battle
      ),

   S1Update =
      btl_character_turn_update:ataxia_set_battle
      (
         UpdatedBattle,
         false,
         BattleAtaxiaUpdate,
         S0Update
      ),

   S1Update.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec handle
   (
      btl_character_turn_update:type(),
      btl_character_turn_request:type()
   )
   -> btl_character_turn_update:type().
handle (Update, Request) ->
   Actions = btl_character_turn_request:get_actions(Request),

   S0Update = handle_actions(Actions, Update),
   S1Update = deactivate_character(S0Update),
   S2Update = update_timeline(S1Update),
   S3Update = btl_turn_progression:handle(S2Update),

   S3Update.
