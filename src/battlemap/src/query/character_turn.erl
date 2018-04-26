-module(character_turn).
% FIXME: There's still too much of a mess in this module.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-include("../../include/yaws_api.hrl").


-record
(
   data,
   {
      battle :: battle:struct(),
      played_character_instance :: character_instance:struct()
   }
).

-record
(
   update,
   {
      updated_data :: data(),
      timeline_update :: list(any()),
      db_update :: list(any())
   }
).

-type data() :: #data{}.
-type update() :: #update{}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([out/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec finalize_and_fuse_data
   (
      data(),
      character_turn_request:type()
   )
   -> battle:struct().
finalize_and_fuse_data (Data, Request) ->
   Battle = Data#data.battle,
   CharacterInstance = Data#data.played_character_instance,

   io:format("~nNot a character instance? ~p~n", [CharacterInstance]),

   FinalizedCharacterInstance =
      character_instance:set_is_active(false, CharacterInstance),

   battle:set_character_instance
   (
      Request#request.character_instance_ix,
      FinalizedCharacterInstance,
      Battle
   ).

%-spec send_to_database (list(database_diff:struct()), character_turn_request:type()) -> 'ok'.
-spec send_to_database (battle:struct(), character_turn_request:type()) -> 'ok'.
send_to_database (FinalizedBattle, Request) ->
   PlayerID = Request#request.player_id,
   BattleID = Request#request.battle_id,

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
   PlayerID = Request#request.player_id,
   BattleID = Request#request.battle_id,

   timed_cache:update
   (
      battle_db,
      PlayerID,
      BattleID,
      Battle
   ).

-spec generate_reply ( list(any())) -> binary().
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
         battle = PostActionBattle,
         played_character_instance = PostActionCharacterInstance
      }
   }.

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

-spec fetch_data (character_turn_request:type()) -> data().
fetch_data (Request) ->
   PlayerID = character_turn_request:get_player_id(Request),
   BattleID = character_turn_request:get_battle_id(Request),
   CharacterInstanceIX =
      character_turn_request:get_character_instance_ix(Request),

   Battle = timed_cache:fetch(battle_db, PlayerID, BattleID),
   CharacterInstance =
      battle:get_character_instance(CharacterInstanceIX, Battle),

   #data
   {
      battle = Battle,
      played_character_instance = CharacterInstance
   }.

-spec assert_user_permissions (data(), character_turn_request:type()) -> true.
assert_user_permissions (Data, Request) ->
   PlayerID = character_turn_request:get_player_id(Request),
   CharacterInstance = Data#data.played_character_instance,
   Battle = Data#data.battle,
   Character = character_instance:get_character(CharacterInstance),
   CurrentPlayerIX =
      player_turn:get_player_ix
      (
         battle:get_current_player_turn(Battle)
      ),
   CurrentPlayer = battle:get_player(CurrentPlayerIX, Battle),
   CharacterOwnerID = character:get_owner_id(Character),

   PlayerID = player:get_id(CurrentPlayer),
   PlayerID = CharacterOwnerID,

   true = character_instance:get_is_active(CharacterInstance).

-spec update_data (data(), character_turn_request:type()) -> update().
update_data (Data, Request) ->
   {ActionsDiffUpdate, ClientUpdate, UpdatedData} =
      handle_actions(Data, Request),

   EncodedClientUpdate = lists:map(fun turn_result:encode/1, ClientUpdate),

   UpdatedBattle = finalize_and_fuse_data(UpdatedData, Request),

   UpdatedBattle2 =
      battle_turn:store_timeline(EncodedClientUpdate, UpdatedBattle),

   {TurnDiffUpdate, FinalizedBattle} =
      battle_turn:handle_post_play(UpdatedBattle2),

   DiffUpdate = (TurnDiffUpdate ++ ActionsDiffUpdate).

-spec commit_update (update(), character_turn_request:type()) -> any().
commit_update (Update, Request) ->
   UpdatedData = Update#update.updated_data,

   % TODO: the database should get a diff update instead.
   send_to_database(UpdatedData, Request),
   update_cache(UpdatedData, Request).

-spec disconnect_user (character_turn_request:type()) -> 'ok'.
disconnect_user (Request) ->
   PlayerID = character_turn_request:get_player_id(Request),

   security:unlock_queries(PlayerID).

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
