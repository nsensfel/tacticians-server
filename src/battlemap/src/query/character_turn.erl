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

%-spec send_to_database (list(database_diff:struct()), character_turn_request:type()) -> 'ok'.
-spec send_to_database (battle:struct(), character_turn_request:type()) -> 'ok'.
send_to_database (FinalizedBattle, Request) ->
   PlayerID = character_turn_request:get_player_id(Request),
   BattleID = character_turn_request:get_battle_id(Request),

   %% TODO: differential commit
   database_shim:commit
   (
      battle_db,
      PlayerID,
      BattleID,
      FinalizedBattle
   ).

-spec update_cache (battle:struct(), character_turn_request:type()) -> 'ok'.
update_cache (Battle, Request) ->
   PlayerID = character_turn_request:get_player_id(Request),
   BattleID = character_turn_request:get_battle_id(Request),

   timed_cache:update
   (
      battle_db,
      PlayerID,
      BattleID,
      Battle
   ).

-spec generate_reply (list(any())) -> binary().
generate_reply (EncodedClientUpdate) ->
   jiffy:encode([turn_results:generate(EncodedClientUpdate)]).

handle_actions (Data, Request) ->
   Battle = Data#data.battle,
   CharacterInstance = Data#data.played_character_instance,
   CharacterInstanceIX = Request#request.character_instance_ix,
   Actions = Request#request.actions,

   {
      ActionsDiffUpdates,
      ClientUpdates,
      PostActionBattle,
      PostActionCharacterInstance
   } =
      lists:foldl
      (
         fun
         (
            Action,
            {
               CurrActionsDiffUpdates,
               CurrClientUpdates,
               CurrBattle,
               CurrCharacterInstance
            }
         ) ->
            {
               NewActionsDiffUpdates,
               NewClientUpdates,
               NewBattle,
               NewCharacterInstance
            } =
               battle_action:handle
               (
                  CurrBattle,
                  CurrCharacterInstance,
                  CharacterInstanceIX,
                  Action
               ),
            {
               (NewActionsDiffUpdates ++ CurrActionsDiffUpdates),
               (NewClientUpdates ++ CurrClientUpdates),
               NewBattle,
               NewCharacterInstance
            }
         end,
         {[], [], Battle, CharacterInstance},
         Actions
      ),
   {
      ActionsDiffUpdates,
      ClientUpdates,
      Data#data
      {
         dirty = true,
         battle = PostActionBattle,
         played_character_instance = PostActionCharacterInstance
      }
   }.


FinalizedCharacterInstance =
   character_instance:set_is_active(false, CharacterInstance),
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec decode_request (binary()) -> character_turn_request:type().
decode_request (BinaryRequest) ->
   JSONMap = jiffy:decode(BinaryRequest, [return_maps]),
   character_turn_request:decode(JSONMap).

-spec authenticate_user (character_turn_request:type()) -> 'ok'.
authenticate_user (Request) ->
   PlayerID = character_turn_request:get_player_id(Request),
   SessionToken = character_turn_request:get_session_token(Request),

   security:assert_identity(PlayerID, SessionToken),
   security:lock_queries(PlayerID).

-spec fetch_data (character_turn_request:type()) -> character_turn_data:type().
fetch_data (Request) ->
   PlayerID = character_turn_request:get_player_id(Request),
   BattleID = character_turn_request:get_battle_id(Request),
   CharacterInstanceIX =
      character_turn_request:get_character_instance_ix(Request),

   Battle = timed_cache:fetch(battle_db, PlayerID, BattleID),

   character_turn_data:new(Battle, CharacterInstanceIX).

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
   CharacterInstance = character_turn_data:get_player_instance(Data),
   Character = character_instance:get_character(CharacterInstance),
   CharacterOwnerID = character:get_owner_id(Character),

   true = (PlayerID == CharacterOwnerID),

   ok.

-spec assert_character_can_be_played (character_turn_data:type()) -> 'ok'.
assert_character_can_be_played (Data) ->
   CharacterInstance = character_turn_data:get_player_instance(Data),

   true = character_instance:get_is_active(CharacterInstance),

   ok.

-spec assert_user_permissions (character_turn_data:type(), character_turn_request:type()) -> 'ok'.
assert_user_permissions (Data, Request) ->
   assert_user_is_current_player(Data, Request),
   assert_user_owns_played_character(Data, Request),
   assert_character_can_be_played(Data),

   ok.

-spec update_data
   (
      character_turn_data:type(),
      character_turn_request:type()
   )
   -> character_turn_update:type().
update_data (Data, Request) ->
   EmptyUpdate = character_turn_update:new(Data),
   PostActionsUpdate = handle_actions(EmptyUpdate, Request),
   NewTurnUpdate = prepare_new_turn(PostActionsUpdate),

   EncodedClientUpdate = lists:map(fun turn_result:encode/1, ClientUpdate),

   UpdatedBattle2 =
      battle_turn:store_timeline(EncodedClientUpdate, UpdatedBattle),

   {TurnDiffUpdate, FinalizedBattle} =
      battle_turn:handle_post_play(UpdatedBattle2),

   DiffUpdate = (TurnDiffUpdate ++ ActionsDiffUpdate).

-spec commit_update
   (
      character_turn_update:type(),
      character_turn_request:type()
   )
   -> 'ok'.
commit_update (Update, Request) ->
   UpdatedData = character_turn_update:get_data(Update),

   % TODO: the database should get a diff update instead.
   send_to_database(UpdatedData, Request),
   update_cache(UpdatedData, Request),

   ok.

-spec disconnect_user (character_turn_request:type()) -> 'ok'.
disconnect_user (Request) ->
   PlayerID = character_turn_request:get_player_id(Request),

   security:unlock_queries(PlayerID),

   ok.

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
