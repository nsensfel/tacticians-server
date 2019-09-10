-module(btl_character_turn).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-include("yaws_api.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([out/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% REQUEST DECODING %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec decode_request (shr_query:type()) -> btl_character_turn_request:type().
decode_request (Query) ->
   btl_character_turn_request:decode(shr_query:get_params(Query)).

%%%% USER AUTHENTICATION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec authenticate_user (btl_character_turn_request:type()) -> ('ok' | 'error').
authenticate_user (Request) ->
   PlayerID = btl_character_turn_request:get_player_id(Request),
   SessionToken = btl_character_turn_request:get_session_token(Request),

   Player = shr_timed_cache:fetch(player_db, ataxia_security:any(), PlayerID),

   case shr_security:credentials_match(SessionToken, Player) of
      true -> ok;
      _ -> error
   end.

%%%% MAIN LOGIC %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec fetch_data
   (
      btl_character_turn_request:type()
   )
   -> btl_character_turn_update:type().
fetch_data (Request) ->
   PlayerID = btl_character_turn_request:get_player_id(Request),
   BattleID = btl_character_turn_request:get_battle_id(Request),
   CharacterIX = btl_character_turn_request:get_character_ix(Request),
   Battle =
      shr_timed_cache:fetch
      (
         battle_db,
         ataxia_security:user_from_id(PlayerID),
         BattleID
      ),

   btl_character_turn_update:new(Battle, CharacterIX).

%%%% ASSERTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec assert_user_is_current_player
   (
      btl_character_turn_update:type(),
      btl_character_turn_request:type()
   ) -> 'ok'.
assert_user_is_current_player (Update, Request) ->
   PlayerID = btl_character_turn_request:get_player_id(Request),
   Battle = btl_character_turn_update:get_battle(Update),
   CurrentPlayerTurn = btl_battle:get_current_player_turn(Battle),
   CurrentPlayerIX = btl_player_turn:get_player_ix(CurrentPlayerTurn),
   CurrentPlayer = btl_battle:get_player(CurrentPlayerIX, Battle),

   case (PlayerID == btl_player:get_id(CurrentPlayer)) of
      true -> ok;
      _ ->
         error
         (
            {
               player_id,
               btl_player:get_id(CurrentPlayer),
               PlayerID
            }
         )
   end.

-spec assert_user_owns_played_character
   (
      btl_character_turn_update:type(),
      btl_character_turn_request:type()
   ) -> 'ok'.
assert_user_owns_played_character (Update, Request) ->
   PlayerID = btl_character_turn_request:get_player_id(Request),
   Battle = btl_character_turn_update:get_battle(Update),
   CharacterIX = btl_character_turn_update:get_character_ix(Update),
   Character = btl_battle:get_character(CharacterIX, Battle),
   CharacterPlayerIX = btl_character:get_player_index(Character),
   CharacterPlayer = btl_battle:get_player(CharacterPlayerIX, Battle),
   CharacterPlayerID = btl_player:get_id(CharacterPlayer),

   case (PlayerID == CharacterPlayerID) of
      true -> ok;
      _ -> error({player_id, CharacterPlayerID, PlayerID})
   end.

-spec assert_character_can_be_played (btl_character_turn_update:type()) -> 'ok'.
assert_character_can_be_played (Update) ->
   Battle = btl_character_turn_update:get_battle(Update),
   CharacterIX = btl_character_turn_update:get_character_ix(Update),
   Character = btl_battle:get_character(CharacterIX, Battle),

   case btl_character:get_is_active(Character) of
      true -> ok;
      _ -> error({character, active, CharacterIX})
   end.

-spec assert_user_permissions
   (
      btl_character_turn_update:type(),
      btl_character_turn_request:type()
   ) -> 'ok'.
assert_user_permissions (Update, Request) ->
   assert_user_is_current_player(Update, Request),
   assert_user_owns_played_character(Update, Request),
   assert_character_can_be_played(Update),

   ok.

%%%% DATABASE UPDATES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec send_to_database
   (
      btl_character_turn_update:type(),
      btl_character_turn_request:type()
   )
   -> 'ok'.
send_to_database (Update, Request) ->
   PlayerID = btl_character_turn_request:get_player_id(Request),
   BattleID = btl_character_turn_request:get_battle_id(Request),
   BattleAtaxicUpdate = btl_character_turn_update:get_battle_update(Update),

   ok =
      ataxia_client:update
      (
         battle_db,
         ataxia_security:user_from_id(PlayerID),
         ataxic:update_value(ataxic:optimize(BattleAtaxicUpdate)),
         BattleID
      ),

   ok.

-spec send_to_cache
   (
      btl_character_turn_update:type(),
      btl_character_turn_request:type()
   )
   -> 'ok'.
send_to_cache (Update, Request) ->
   PlayerID = btl_character_turn_request:get_player_id(Request),
   BattleID = btl_character_turn_request:get_battle_id(Request),
   Battle = btl_character_turn_update:get_battle(Update),

   shr_timed_cache:update
   (
      battle_db,
      ataxia_security:user_from_id(PlayerID),
      BattleID,
      Battle
   ),

   ok.

-spec commit_update
   (
      btl_character_turn_update:type(),
      btl_character_turn_request:type()
   )
   -> 'ok'.
commit_update (Update, Request) ->
   send_to_database(Update, Request),
   send_to_cache(Update, Request),

   ok.

%%%% REPLY GENERATION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec generate_reply (btl_character_turn_update:type()) -> binary().
generate_reply (Update) ->
   NewTimelineItems = btl_character_turn_update:get_timeline(Update),

   TurnResultReply = btl_turn_results:generate(NewTimelineItems),

   jiffy:encode([TurnResultReply]).

%%%% MAIN LOGIC %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec deactivate_character
   (
      btl_character_turn_update:type()
   )
   -> btl_character_turn_update:type().
deactivate_character (S0Update) ->
   Battle = btl_character_turn_update:get_battle(S0Update),
   CharacterIX = btl_character_turn_update:get_character_ix(S0Update),
   Character = btl_battle:get_character(CharacterIX, Battle),

   {UpdatedCharacter, CharacterAtaxiaUpdate} =
      btl_character:ataxia_set_is_active(false, Character),

   {UpdatedBattle, BattleAtaxiaUpdate} =
      btl_battle:ataxia_set_character
      (
         CharacterIX,
         UpdatedCharacter,
         CharacterAtaxiaUpdate,
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

-spec update_timeline
   (
      btl_character_turn_update:type()
   )
   -> btl_character_turn_update:type().
update_timeline (S0Update) ->
   NewTimelineElements = btl_character_turn_update:get_timeline(S0Update),
   Battle = btl_character_turn_update:get_battle(S0Update),
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

-spec handle (shr_query:type()) -> binary().
handle (Query) ->
   Request = decode_request(Query),
   case authenticate_user(Request) of
      ok ->
         PlayerID = btl_character_turn_request:get_player_id(Request),

         shr_security:lock_queries(PlayerID),

         S0Update = fetch_data(Request),
         assert_user_permissions(S0Update, Request),
         S1Update = btl_actions_management:handle
         (
            btl_character_turn_request:get_actions(Request),
            S0Update
         ),
         S2Update = deactivate_character(S1Update),
         S3Update = update_timeline(S2Update),
         S4Update = btl_turn_progression:handle(S3Update),

         commit_update(S4Update, Request),

         shr_security:unlock_queries(PlayerID),

         generate_reply(S4Update);

      error -> jiffy:encode([shr_disconnected:generate()])
   end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
out(A) ->
   {
      content,
      "application/json; charset=UTF-8",
      handle(shr_query:new(A))
   }.
