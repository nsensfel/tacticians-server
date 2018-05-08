-module(character_turn).
% FIXME: There's still too much of a mess in this module.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-include("../../include/yaws_api.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([out/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%-spec send_to_database (list(database_diff:type()), character_turn_request:type()) -> 'ok'.


%%%% REQUEST DECODING %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec decode_request (binary()) -> character_turn_request:type().
decode_request (BinaryRequest) ->
   JSONMap = jiffy:decode(BinaryRequest, [return_maps]),

   character_turn_request:decode(JSONMap).

%%%% USER AUTHENTICATION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec authenticate_user (character_turn_request:type()) -> 'ok'.
authenticate_user (Request) ->
   PlayerID = character_turn_request:get_player_id(Request),
   SessionToken = character_turn_request:get_session_token(Request),

   security:assert_identity(PlayerID, SessionToken),
   security:lock_queries(PlayerID),

   ok.

%%%% MAIN LOGIC %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec fetch_data (character_turn_request:type()) -> character_turn_data:type().
fetch_data (Request) ->
   PlayerID = character_turn_request:get_player_id(Request),
   BattleID = character_turn_request:get_battle_id(Request),
   CharacterInstanceIX =
      character_turn_request:get_character_instance_ix(Request),

   Battle = timed_cache:fetch(battle_db, PlayerID, BattleID),

   character_turn_data:new(Battle, CharacterInstanceIX).

%%%% ASSERTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec assert_user_is_current_player
   (
      character_turn_data:type(),
      character_turn_request:type()
   ) -> 'ok'.
assert_user_is_current_player (Data, Request) ->
   PlayerID = character_turn_request:get_player_id(Request),
   Battle = character_turn_data:get_battle(Data),
   CurrentPlayerTurn = battle:get_current_player_turn(Battle),
   CurrentPlayerIX = player_turn:get_player_ix(CurrentPlayerTurn),
   CurrentPlayer = battle:get_player(CurrentPlayerIX, Battle),

   true = (PlayerID == player:get_id(CurrentPlayer)),

   ok.

-spec assert_user_owns_played_character
   (
      character_turn_data:type(),
      character_turn_request:type()
   ) -> 'ok'.
assert_user_owns_played_character (Data, Request) ->
   PlayerID = character_turn_request:get_player_id(Request),
   CharacterInstance = character_turn_data:get_character_instance(Data),
   Character = character_instance:get_character(CharacterInstance),
   CharacterOwnerID = character:get_owner_id(Character),

   true = (PlayerID == CharacterOwnerID),

   ok.

-spec assert_character_can_be_played (character_turn_data:type()) -> 'ok'.
assert_character_can_be_played (Data) ->
   CharacterInstance = character_turn_data:get_character_instance(Data),

   true = character_instance:get_is_active(CharacterInstance),

   ok.

-spec assert_user_permissions
   (
      character_turn_data:type(),
      character_turn_request:type()
   ) -> 'ok'.
assert_user_permissions (Data, Request) ->
   assert_user_is_current_player(Data, Request),
   assert_user_owns_played_character(Data, Request),
   assert_character_can_be_played(Data),

   ok.

%%%% QUERY LOGIC HANDLING %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec finalize_character_instance
   (
      character_turn_update:type()
   )
   -> character_turn_update:type().
finalize_character_instance (Update) ->
   Data = character_turn_update:get_data(Update),
   CharacterInstance = character_turn_data:get_character_instance(Data),

   DisabledCharacterInstance =
      character_instance:set_is_active(false, CharacterInstance),

   UpdatedData =
      character_turn_data:set_character_instance
      (
         DisabledCharacterInstance,
         Data
      ),
   FinalizedData = character_turn_data:cleanup(UpdatedData),

   character_turn_update:set_data(FinalizedData, Update).

-spec handle_actions
   (
      character_turn_data:type(),
      character_turn_request:type()
   )
   -> character_turn_update:type().
handle_actions (Data, Request) ->
   Actions = character_turn_request:get_actions(Request),

   EmptyUpdate = character_turn_update:new(Data),
   PostActionsUpdate =
      lists:foldl(fun turn_actions:handle/2, EmptyUpdate, Actions),

   finalize_character_instance(PostActionsUpdate).

-spec update_timeline
   (
      character_turn_update:type()
   )
   -> character_turn_update:type().
update_timeline (Update) ->
   NewTimelineElements = character_turn_update:get_timeline(Update),
   Data = character_turn_update:get_data(Update),
   Battle = character_turn_data:get_battle(Data),
   PlayerTurn = battle:get_current_player_turn(Battle),
   PlayerIX = player_turn:get_player_ix(PlayerTurn),
   Player = battle:get_player(PlayerIX, Battle),

   UpdatedPlayer = player:add_to_timeline(NewTimelineElements, Player),
   UpdatedBattle = battle:set_player(PlayerIX, UpdatedPlayer, Battle),
   UpdatedData = character_turn_data:set_battle(UpdatedBattle, Data),

   character_turn_update:set_data(UpdatedData, Update).

-spec update_data
   (
      character_turn_data:type(),
      character_turn_request:type()
   )
   -> character_turn_update:type().
update_data (Data, Request) ->
   EmptyUpdate = character_turn_update:new(Data),
   PostActionsUpdate = handle_actions(EmptyUpdate, Request),
   PostCharacterTurnUpdate = update_timeline(PostActionsUpdate),

   next_turn:update_if_needed(PostCharacterTurnUpdate).

%%%% DATABASE UPDATES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec send_to_database
   (
      character_turn_update:type(),
      character_turn_request:type()
   )
   -> 'ok'.
send_to_database (Update, Request) ->
   PlayerID = character_turn_request:get_player_id(Request),
   BattleID = character_turn_request:get_battle_id(Request),
   Data = character_turn_update:get_data(Update),
   Battle = character_turn_data:get_battle(Data),

   %% TODO: differential commit
   database_shim:commit
   (
      battle_db,
      PlayerID,
      BattleID,
      Battle
   ),

   ok.

-spec send_to_cache
   (
      character_turn_update:type(),
      character_turn_request:type()
   )
   -> 'ok'.
send_to_cache (Update, Request) ->
   PlayerID = character_turn_request:get_player_id(Request),
   BattleID = character_turn_request:get_battle_id(Request),
   Data = character_turn_update:get_data(Update),
   Battle = character_turn_data:get_battle(Data),

   timed_cache:update
   (
      battle_db,
      PlayerID,
      BattleID,
      Battle
   ),

   ok.

-spec commit_update
   (
      character_turn_update:type(),
      character_turn_request:type()
   )
   -> 'ok'.
commit_update (Update, Request) ->
   send_to_database(Update, Request),
   send_to_cache(Update, Request),

   ok.

%%%% USER DISCONNECTION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec disconnect_user (character_turn_request:type()) -> 'ok'.
disconnect_user (Request) ->
   PlayerID = character_turn_request:get_player_id(Request),

   security:unlock_queries(PlayerID),

   ok.

%%%% REPLY GENERATION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec generate_reply (character_turn_update:data()) -> binary().
generate_reply (Update) ->
   NewTimelineItems = character_turn_update:get_timeline(Update),

   TurnResultReply = turn_results:generate(NewTimelineItems),

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
