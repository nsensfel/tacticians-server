-module(character_turn).
% FIXME: There's still too much of a mess in this module.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-include("../../include/yaws_api.hrl").

-record
(
   input,
   {
      player_id :: player:id(),
      session_token :: binary(),
      battle_id :: binary(),
      character_instance_ix :: non_neg_integer(),
      actions :: list(battle_action:struct())
   }
).

-record
(
   relevant_data,
   {
      battle :: battle:struct(),
      played_character_instance :: character_instance:struct()
   }
).

-type input() :: #input{}.
-type relevant_data() :: #relevant_data{}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([out/1]).

-export_type([relevant_data/0]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec parse_input (binary()) -> input().
parse_input (Req) ->
   JSONReqMap = jiffy:decode(Req, [return_maps]),
   CharacterInstanceIX = binary_to_integer(maps:get(<<"cix">>, JSONReqMap)),
   EncodedActions = maps:get(<<"act">>, JSONReqMap),
   Actions = lists:map(fun battle_action:decode/1, EncodedActions),

   #input
   {
      player_id = maps:get(<<"pid">>, JSONReqMap),
      session_token = maps:get(<<"stk">>, JSONReqMap),
      battle_id = maps:get(<<"bid">>, JSONReqMap),
      character_instance_ix = CharacterInstanceIX,
      actions = Actions
   }.

-spec fetch_relevant_data (input()) -> relevant_data().
fetch_relevant_data (Input) ->
   PlayerID = Input#input.player_id,
   BattleID = Input#input.battle_id,
   CharacterInstanceIX = Input#input.character_instance_ix,

   Battle = timed_cache:fetch(battle_db, PlayerID, BattleID),
   CharacterInstance =
      battle:get_character_instance(CharacterInstanceIX, Battle),

   #relevant_data
   {
      battle = Battle,
      played_character_instance = CharacterInstance
   }.

-spec assert_character_instance_can_be_played
   (
      relevant_data(),
      input()
   )
   -> true.
assert_character_instance_can_be_played (RData, Input) ->
   PlayerID = Input#input.player_id,
   CharacterInstance = RData#relevant_data.played_character_instance,
   Battle = RData#relevant_data.battle,
   Character = character_instance:get_character(CharacterInstance),
   CurrentPlayerIX =
      player_turn:get_player_ix
      (
         battle:get_current_player_turn(Battle)
      ),
   CurrentPlayer = battle:get_player_id(CurrentPlayerIX, Battle),
   CharacterOwner = character:get_owner_id(Character),

   PlayerID = CurrentPlayer,
   PlayerID = CharacterOwner,

   true = character_instance:get_is_active(CharacterInstance).

-spec finalize_and_fuse_relevant_data
   (
      relevant_data(),
      input()
   )
   -> battle:struct().
finalize_and_fuse_relevant_data (RData, Input) ->
   Battle = RData#relevant_data.battle,
   CharacterInstance = RData#relevant_data.played_character_instance,

   io:format("~nNot a character instance? ~p~n", [CharacterInstance]),

   FinalizedCharacterInstance =
      character_instance:set_is_active(false, CharacterInstance),

   battle:set_character_instance
   (
      Input#input.character_instance_ix,
      FinalizedCharacterInstance,
      Battle
   ).

%-spec send_to_database (list(database_diff:struct()), input()) -> 'ok'.
-spec send_to_database (battle:struct(), input()) -> 'ok'.
send_to_database (FinalizedBattle, Input) ->
   PlayerID = Input#input.player_id,
   BattleID = Input#input.battle_id,

   %% TODO: differential commit
   database_shim:commit
   (
      battle_db,
      PlayerID,
      BattleID,
      FinalizedBattle
   ).

-spec update_cache (battle:struct(), input()) -> 'ok'.
update_cache (Battle, Input) ->
   PlayerID = Input#input.player_id,
   BattleID = Input#input.battle_id,

   timed_cache:update
   (
      battle_db,
      PlayerID,
      BattleID,
      Battle
   ).

-spec generate_reply
   (
      list(turn_result:struct())
   )
   -> binary().
generate_reply (ClientUpdate) ->
   %% TODO
   jiffy:encode
   (
      [
         [
            <<"raw">>,
            lists:map(fun turn_result:encode/1, ClientUpdate)
         ]
      ]
   ).

handle_actions (RData, Input) ->
   Battle = RData#relevant_data.battle,
   CharacterInstance= RData#relevant_data.played_character_instance,
   CharacterInstanceIX = Input#input.character_instance_ix,
   Actions = Input#input.actions,

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
               (CurrActionsDiffUpdates ++ NewActionsDiffUpdates),
               (CurrClientUpdates ++ NewClientUpdates),
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
      RData#relevant_data
      {
         battle = PostActionBattle,
         played_character_instance = PostActionCharacterInstance
      }
   }.

-spec handle (binary()) -> binary().
handle (Req) ->
   Input = parse_input(Req),
   PlayerID = Input#input.player_id,
   PlayerSessionToken = Input#input.session_token,

   security:assert_identity(PlayerID, PlayerSessionToken),
   security:lock_queries(PlayerID),

   RData = fetch_relevant_data(Input),

   assert_character_instance_can_be_played(RData, Input),

   {ActionsDiffUpdate, ClientUpdate, UpdatedRData} =
      handle_actions(RData, Input),

   UpdatedBattle = finalize_and_fuse_relevant_data(UpdatedRData, Input),

   {TurnDiffUpdate, FinalizedBattle} =
      battle_turn:handle_post_play(UpdatedBattle),

   DiffUpdate = (TurnDiffUpdate ++ ActionsDiffUpdate),

   %send_to_database(DiffUpdate, Input),
   send_to_database(FinalizedBattle, Input),
   update_cache(FinalizedBattle, Input),

   io:format("~nCharacter turn result:~n~p~n", [DiffUpdate]),

   security:unlock_queries(PlayerID),

   generate_reply(ClientUpdate).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
out(A) ->
   {
      content,
      "application/json; charset=UTF-8",
      handle(A#arg.clidata)
   }.
