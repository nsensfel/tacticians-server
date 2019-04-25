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

-spec handle_action
(
   btl_battle_action:type(),
   btl_character_turn_update:type()
)
-> btl_character_turn_update:type().
handle_action (BattleAction, Update) ->
   case btl_battle_action:get_category(BattleAction) of
      move -> btl_turn_actions_move:handle(BattleAction, Update);
      switch_weapon -> btl_turn_actions_switch_weapon:handle(Update);
      attack -> btl_turn_actions_attack:handle(BattleAction, Update)
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

   S0Update = lists:foldl(fun handle_action/2, Update, Actions),
   S1Update = deactivate_character(S0Update),
   S2Update = update_timeline(S1Update),
   S3Update = btl_turn_progression:handle(S2Update),

   S3Update.
