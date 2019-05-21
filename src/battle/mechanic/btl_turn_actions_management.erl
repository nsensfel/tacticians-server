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

-spec main_character_is_alive
   (
      btl_character_turn_update:type()
   )
   -> {boolean(), btl_character_turn_update:type()}.
main_character_is_alive (Update) ->
   {S0Update, MainCharacter} = btl_character_turn_update:get_character(Update),
   {btl_character:get_is_alive(MainCharacter), S0Update}.

-spec handle_actions
   (
      list(btl_action:type()),
      btl_character_turn_update:type()
   )
   -> btl_character_turn_update:type().
handle_actions ([], Update) -> Update;
handle_actions ([BattleAction|FutureBattleActions], Update) ->
   {MainCharacterIsAlive, S0Update} = main_character_is_alive(Update),

   ActionResult =
      case {MainCharacterIsAlive, btl_action:get_category(BattleAction)} of
         {false, _} -> {ok, S0Update};
         {true, move} -> btl_turn_actions_move:handle(BattleAction, S0Update);
         {true, switch_weapon} ->
            btl_turn_actions_switch_weapon:handle(S0Update);
         {true, attack} ->
            btl_turn_actions_attack:handle(BattleAction, S0Update);
         {true, interrupted_move} ->
            btl_turn_actions_move:handle(BattleAction, S0Update);
         {true, defend} ->
            % TODO: Attack of Opportunity
            Update
      end,

   case ActionResult of
      {ok, NewUpdate} -> handle_actions(FutureBattleActions, NewUpdate);
      {events, NewEvents, NewUpdate} ->
         handle_actions
         (
            (NewEvents ++ FutureBattleActions),
            NewUpdate
         )
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
