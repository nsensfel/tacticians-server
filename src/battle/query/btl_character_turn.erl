-module(btl_character_turn).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-include("../../../include/yaws_api.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([out/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%-spec send_to_database (list(database_diff:type()), character_turn_request:type()) -> 'ok'.


%%%% REQUEST DECODING %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec decode_request (binary()) -> btl_character_turn_request:type().
decode_request (BinaryRequest) ->
   JSONMap = jiffy:decode(BinaryRequest, [return_maps]),

   btl_character_turn_request:decode(JSONMap).

%%%% USER AUTHENTICATION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec authenticate_user (btl_character_turn_request:type()) -> ('ok' | 'error').
authenticate_user (Request) ->
   PlayerID = btl_character_turn_request:get_player_id(Request),
   SessionToken = btl_character_turn_request:get_session_token(Request),

   Player = shr_timed_cache:fetch(player_db, any, PlayerID),

   case shr_security:credentials_match(SessionToken, Player) of
      true -> ok;
      _ -> error
   end.

%%%% MAIN LOGIC %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec fetch_data
   (
      btl_character_turn_request:type()
   )
   -> btl_character_turn_data:type().
fetch_data (Request) ->
   PlayerID = btl_character_turn_request:get_player_id(Request),
   BattleID = btl_character_turn_request:get_battle_id(Request),
   CharacterIX = btl_character_turn_request:get_character_ix(Request),
   Battle = shr_timed_cache:fetch(battle_db, PlayerID, BattleID),

   btl_character_turn_data:new(Battle, CharacterIX).

%%%% ASSERTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec assert_user_is_current_player
   (
      btl_character_turn_data:type(),
      btl_character_turn_request:type()
   ) -> 'ok'.
assert_user_is_current_player (Data, Request) ->
   PlayerID = btl_character_turn_request:get_player_id(Request),
   Battle = btl_character_turn_data:get_battle(Data),
   CurrentPlayerTurn = btl_battle:get_current_player_turn(Battle),
   CurrentPlayerIX = btl_player_turn:get_player_ix(CurrentPlayerTurn),
   CurrentPlayer = btl_battle:get_player(CurrentPlayerIX, Battle),

   true = (PlayerID == btl_player:get_id(CurrentPlayer)),

   ok.

-spec assert_user_owns_played_character
   (
      btl_character_turn_data:type(),
      btl_character_turn_request:type()
   ) -> 'ok'.
assert_user_owns_played_character (Data, Request) ->
   PlayerID = btl_character_turn_request:get_player_id(Request),
   Battle = btl_character_turn_data:get_battle(Data),
   Players = btl_battle:get_players(Battle),
   Character = btl_character_turn_data:get_character(Data),
   CharacterPlayerIX = btl_character:get_player_index(Character),
   CharacterPlayer = array:get(CharacterPlayerIX, Players),
   CharacterPlayerID = btl_player:get_id(CharacterPlayer),

   true = (PlayerID == CharacterPlayerID),

   ok.

-spec assert_character_can_be_played (btl_character_turn_data:type()) -> 'ok'.
assert_character_can_be_played (Data) ->
   Character = btl_character_turn_data:get_character(Data),

   true = btl_character:get_is_active(Character),

   ok.

-spec assert_user_permissions
   (
      btl_character_turn_data:type(),
      btl_character_turn_request:type()
   ) -> 'ok'.
assert_user_permissions (Data, Request) ->
   assert_user_is_current_player(Data, Request),
   assert_user_owns_played_character(Data, Request),
   assert_character_can_be_played(Data),

   ok.

%%%% QUERY LOGIC HANDLING %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
      shr_db_query:update_indexed
      (
         btl_battle:get_characters_field(),
         btl_character_turn_data:get_character_ix(Data),
         [ shr_db_query:set_field(btl_character:get_is_active_field(), false) ]
      ),

   S0Update = btl_character_turn_update:set_data(FinalizedData, Update),
   S1Update = btl_character_turn_update:add_to_db(DBQuery, S0Update),

   S1Update.

-spec handle_actions
   (
      btl_character_turn_data:type(),
      btl_character_turn_request:type()
   )
   -> btl_character_turn_update:type().
handle_actions (Data, Request) ->
   Actions = btl_character_turn_request:get_actions(Request),

   EmptyUpdate = btl_character_turn_update:new(Data),
   PostActionsUpdate =
      lists:foldl(fun btl_turn_actions:handle/2, EmptyUpdate, Actions),

   finalize_character(PostActionsUpdate).

-spec update_timeline
   (
      btl_character_turn_update:type()
   )
   -> btl_character_turn_update:type().
update_timeline (Update) ->
   NewTimelineElements = btl_character_turn_update:get_timeline(Update),
   Data = btl_character_turn_update:get_data(Update),
   Battle = btl_character_turn_data:get_battle(Data),
   PlayerTurn = btl_battle:get_current_player_turn(Battle),
   PlayerIX = btl_player_turn:get_player_ix(PlayerTurn),
   Player = btl_battle:get_player(PlayerIX, Battle),

   UpdatedPlayer = btl_player:add_to_timeline(NewTimelineElements, Player),
   UpdatedBattle = btl_battle:set_player(PlayerIX, UpdatedPlayer, Battle),
   UpdatedData = btl_character_turn_data:set_battle(UpdatedBattle, Data),

   DBQuery =
      shr_db_query:update_indexed
      (
         btl_battle:get_players_field(),
         PlayerIX,
         [
            shr_db_query:add_to_field
            (
               btl_player:get_timeline_field(),
               NewTimelineElements,
               true % We add those to the start of the list
            )
         ]
      ),

   S0Update = btl_character_turn_update:set_data(UpdatedData, Update),
   S1Update = btl_character_turn_update:add_to_db(DBQuery, S0Update),

   S1Update.


-spec update_data
   (
      btl_character_turn_data:type(),
      btl_character_turn_request:type()
   )
   -> btl_character_turn_update:type().
update_data (Data, Request) ->
   PostActionsUpdate = handle_actions(Data, Request),
   PostCharacterTurnUpdate = update_timeline(PostActionsUpdate),

   btl_next_turn:update_if_needed(PostCharacterTurnUpdate).

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
   Ops = btl_character_turn_update:get_db(Update),
   Query = shr_db_query:new(battle_db, BattleID, {user, PlayerID}, Ops),

   shr_database:commit(Query),

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
   Data = btl_character_turn_update:get_data(Update),
   Battle = btl_character_turn_data:get_battle(Data),

   shr_timed_cache:update(battle_db, PlayerID, BattleID, Battle),

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

%%%% USER DISCONNECTION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec disconnect_user (btl_character_turn_request:type()) -> 'ok'.
disconnect_user (Request) ->
   PlayerID = btl_character_turn_request:get_player_id(Request),

   shr_security:unlock_queries(PlayerID),

   ok.

%%%% REPLY GENERATION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec generate_reply (btl_character_turn_update:type()) -> binary().
generate_reply (Update) ->
   NewTimelineItems = btl_character_turn_update:get_timeline(Update),

   TurnResultReply = btl_turn_results:generate(NewTimelineItems),

   jiffy:encode([TurnResultReply]).

%%%% MAIN LOGIC %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec handle (binary()) -> binary().
handle (EncodedRequest) ->
   Request = decode_request(EncodedRequest),
   case authenticate_user(Request) of
      ok ->
         PlayerID = btl_character_turn_request:get_player_id(Request),
         shr_security:lock_queries(PlayerID),
         Data = fetch_data(Request),
         assert_user_permissions(Data, Request),
         Update = update_data(Data, Request),
         commit_update(Update, Request),
         disconnect_user(Request),
         generate_reply(Update);

      error -> jiffy:encode([shr_disconnected:generate()])
   end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
out(A) ->
   {
      content,
      "application/json; charset=UTF-8",
      handle(A#arg.clidata)
   }.
