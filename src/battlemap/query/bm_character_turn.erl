-module(bm_character_turn).

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
-spec decode_request (binary()) -> bm_character_turn_request:type().
decode_request (BinaryRequest) ->
   JSONMap = jiffy:decode(BinaryRequest, [return_maps]),

   bm_character_turn_request:decode(JSONMap).

%%%% USER AUTHENTICATION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec authenticate_user (bm_character_turn_request:type()) -> 'ok'.
authenticate_user (Request) ->
   PlayerID = bm_character_turn_request:get_player_id(Request),
   SessionToken = bm_character_turn_request:get_session_token(Request),

   bm_security:assert_identity(PlayerID, SessionToken),
   bm_security:lock_queries(PlayerID),

   ok.

%%%% MAIN LOGIC %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec fetch_data
   (
      bm_character_turn_request:type()
   )
   -> bm_character_turn_data:type().
fetch_data (Request) ->
   PlayerID = bm_character_turn_request:get_player_id(Request),
   BattleID = bm_character_turn_request:get_battle_id(Request),
   CharacterIX = bm_character_turn_request:get_character_ix(Request),
   Battle = sh_timed_cache:fetch(battle_db, PlayerID, BattleID),

   bm_character_turn_data:new(Battle, CharacterIX).

%%%% ASSERTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec assert_user_is_current_player
   (
      bm_character_turn_data:type(),
      bm_character_turn_request:type()
   ) -> 'ok'.
assert_user_is_current_player (Data, Request) ->
   PlayerID = bm_character_turn_request:get_player_id(Request),
   Battle = bm_character_turn_data:get_battle(Data),
   CurrentPlayerTurn = bm_battle:get_current_player_turn(Battle),
   CurrentPlayerIX = bm_player_turn:get_player_ix(CurrentPlayerTurn),
   CurrentPlayer = bm_battle:get_player(CurrentPlayerIX, Battle),

   true = (PlayerID == bm_player:get_id(CurrentPlayer)),

   ok.

-spec assert_user_owns_played_character
   (
      bm_character_turn_data:type(),
      bm_character_turn_request:type()
   ) -> 'ok'.
assert_user_owns_played_character (Data, Request) ->
   PlayerID = bm_character_turn_request:get_player_id(Request),
   Battle = bm_character_turn_data:get_battle(Data),
   Players = bm_battle:get_players(Battle),
   Character = bm_character_turn_data:get_character(Data),
   CharacterPlayerIX = bm_character:get_player_index(Character),
   CharacterPlayer = array:get(CharacterPlayerIX, Players),
   CharacterPlayerID = bm_player:get_id(CharacterPlayer),

   true = (PlayerID == CharacterPlayerID),

   ok.

-spec assert_character_can_be_played (bm_character_turn_data:type()) -> 'ok'.
assert_character_can_be_played (Data) ->
   Character = bm_character_turn_data:get_character(Data),

   true = bm_character:get_is_active(Character),

   ok.

-spec assert_user_permissions
   (
      bm_character_turn_data:type(),
      bm_character_turn_request:type()
   ) -> 'ok'.
assert_user_permissions (Data, Request) ->
   assert_user_is_current_player(Data, Request),
   assert_user_owns_played_character(Data, Request),
   assert_character_can_be_played(Data),

   ok.

%%%% QUERY LOGIC HANDLING %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec finalize_character
   (
      bm_character_turn_update:type()
   )
   -> bm_character_turn_update:type().
finalize_character (Update) ->
   Data = bm_character_turn_update:get_data(Update),
   Character = bm_character_turn_data:get_character(Data),

   DisabledCharacter = bm_character:set_is_active(false, Character),
   UpdatedData = bm_character_turn_data:set_character(DisabledCharacter, Data),
   FinalizedData = bm_character_turn_data:clean_battle(UpdatedData),

   DBQuery =
      sh_db_query:update_indexed
      (
         bm_battle:get_characters_field(),
         bm_character_turn_data:get_character_ix(Data),
         [ sh_db_query:set_field(bm_character:get_is_active_field(), false) ]
      ),

   S0Update = bm_character_turn_update:set_data(FinalizedData, Update),
   S1Update = bm_character_turn_update:add_to_db(DBQuery, S0Update),

   S1Update.

-spec handle_actions
   (
      bm_character_turn_data:type(),
      bm_character_turn_request:type()
   )
   -> bm_character_turn_update:type().
handle_actions (Data, Request) ->
   Actions = bm_character_turn_request:get_actions(Request),

   EmptyUpdate = bm_character_turn_update:new(Data),
   PostActionsUpdate =
      lists:foldl(fun bm_turn_actions:handle/2, EmptyUpdate, Actions),

   finalize_character(PostActionsUpdate).

-spec update_timeline
   (
      bm_character_turn_update:type()
   )
   -> bm_character_turn_update:type().
update_timeline (Update) ->
   NewTimelineElements = bm_character_turn_update:get_timeline(Update),
   Data = bm_character_turn_update:get_data(Update),
   Battle = bm_character_turn_data:get_battle(Data),
   PlayerTurn = bm_battle:get_current_player_turn(Battle),
   PlayerIX = bm_player_turn:get_player_ix(PlayerTurn),
   Player = bm_battle:get_player(PlayerIX, Battle),

   UpdatedPlayer = bm_player:add_to_timeline(NewTimelineElements, Player),
   UpdatedBattle = bm_battle:set_player(PlayerIX, UpdatedPlayer, Battle),
   UpdatedData = bm_character_turn_data:set_battle(UpdatedBattle, Data),

   DBQuery =
      sh_db_query:update_indexed
      (
         bm_battle:get_players_field(),
         PlayerIX,
         [
            sh_db_query:add_to_field
            (
               bm_player:get_timeline_field(),
               NewTimelineElements,
               true % We add those to the start of the list
            )
         ]
      ),

   S0Update = bm_character_turn_update:set_data(UpdatedData, Update),
   S1Update = bm_character_turn_update:add_to_db(DBQuery, S0Update),

   S1Update.


-spec update_data
   (
      bm_character_turn_data:type(),
      bm_character_turn_request:type()
   )
   -> bm_character_turn_update:type().
update_data (Data, Request) ->
   PostActionsUpdate = handle_actions(Data, Request),
   PostCharacterTurnUpdate = update_timeline(PostActionsUpdate),

   bm_next_turn:update_if_needed(PostCharacterTurnUpdate).

%%%% DATABASE UPDATES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec send_to_database
   (
      bm_character_turn_update:type(),
      bm_character_turn_request:type()
   )
   -> 'ok'.
send_to_database (Update, Request) ->
   PlayerID = bm_character_turn_request:get_player_id(Request),
   BattleID = bm_character_turn_request:get_battle_id(Request),
   Ops = bm_character_turn_update:get_db(Update),
   Query = sh_db_query:new(battle_db, BattleID, {user, PlayerID}, Ops),

   sh_database:commit(Query),

   ok.

-spec send_to_cache
   (
      bm_character_turn_update:type(),
      bm_character_turn_request:type()
   )
   -> 'ok'.
send_to_cache (Update, Request) ->
   PlayerID = bm_character_turn_request:get_player_id(Request),
   BattleID = bm_character_turn_request:get_battle_id(Request),
   Data = bm_character_turn_update:get_data(Update),
   Battle = bm_character_turn_data:get_battle(Data),

   sh_timed_cache:update(battle_db, PlayerID, BattleID, Battle),

   ok.

-spec commit_update
   (
      bm_character_turn_update:type(),
      bm_character_turn_request:type()
   )
   -> 'ok'.
commit_update (Update, Request) ->
   send_to_database(Update, Request),
   send_to_cache(Update, Request),

   ok.

%%%% USER DISCONNECTION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec disconnect_user (bm_character_turn_request:type()) -> 'ok'.
disconnect_user (Request) ->
   PlayerID = bm_character_turn_request:get_player_id(Request),

   bm_security:unlock_queries(PlayerID),

   ok.

%%%% REPLY GENERATION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec generate_reply (bm_character_turn_update:type()) -> binary().
generate_reply (Update) ->
   NewTimelineItems = bm_character_turn_update:get_timeline(Update),

   TurnResultReply = bm_turn_results:generate(NewTimelineItems),

   jiffy:encode([TurnResultReply]).

%%%% MAIN LOGIC %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec handle (binary()) -> binary().
handle (EncodedRequest) ->
   Request = decode_request(EncodedRequest),
   authenticate_user(Request),
   Data = fetch_data(Request),
   assert_user_permissions(Data, Request),
   Update = update_data(Data, Request),
   commit_update(Update, Request),
   disconnect_user(Request),
   generate_reply(Update).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
out(A) ->
   {
      content,
      "application/json; charset=UTF-8",
      handle(A#arg.clidata)
   }.
