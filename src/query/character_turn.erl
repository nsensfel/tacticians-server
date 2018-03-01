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

-record
(
   query_result,
   {
      is_new_turn :: boolean(),
      updated_character_instance_ixs :: list(non_neg_integer()),
      updated_battle :: battle:struct()
   }
).

-type input() :: #input{}.
-type query_result() :: #query_result{}.

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

-spec fetch_relevant_data (input()) -> battle:struct().
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

   FinalizedCharacterInstance =
      character_instance:set_is_active(false, CharacterInstance),

   battle:set_character_instance
   (
      Input#input.character_instance_ix,
      FinalizedCharacterInstance,
      Battle
   ).

-spec activate_relevant_character_instances
   (
      list(non_neg_integer()),
      array:array(character_instance:struct()),
      player:id(),
      (-1 | non_neg_integer())
   )
   -> {list(non_neg_integer()), array:array(character_instance:struct())}.
activate_relevant_character_instances (IXs, CharacterInstances, _Owner, -1) ->
   {IXs, CharacterInstances};
activate_relevant_character_instances (IXs, CharacterInstances, Owner, IX) ->
   CharacterInstance = array:get(IX, CharacterInstances),
   Character = character_instance:get_character(CharacterInstance),
   case character:get_owner_id(Character) of
      OwnerID when (OwnerID == Owner) ->
         activate_relevant_character_instances
         (
            [IX|IXs],
            array:set
            (
               IX,
               character_instance:set_is_active(true, CharacterInstance),
               CharacterInstances
            ),
            Owner,
            (IX - 1)
         );

      _ ->
         activate_relevant_character_instances
         (
            IXs,
            CharacterInstances,
            Owner,
            (IX - 1)
         )
   end.

-spec start_next_players_turn
   (
      query_state()
   )
   -> {list(non_neg_integer()), battle:struct()}.
start_next_players_turn (QueryState) ->
   Battle = QueryState#query_state.battle,
   PlayerIDs = battle:get_player_ids(Battle),
   PlayerTurn = battle:get_current_player_turn(Battle),
   CurrentPlayerIX = player_turn:get_player_ix(PlayerTurn),
   CurrentTurnNumber = player_turn:get_number(PlayerTurn),
   CharacterInstances = battle:get_character_instances(Battle),

   NextPlayerIX = ((CurrentPlayerIX + 1) rem (array:size(PlayerIDs))),
   NextPlayerTurn =
      player_turn:new
      (
         case NextPlayerIX of
            0 -> (CurrentTurnNumber + 1);
            _ -> CurrentTurnNumber
         end,
         NextPlayerIX
      ),

   {ActivatedCharacterInstanceIXs, UpdatedCharacterInstances} =
      activate_relevant_character_instances
      (
         [],
         CharacterInstances,
         array:get(NextPlayerIX, PlayerIDs),
         (array:size(CharacterInstances) - 1)
      ),
   UpdatedBattle =
      battle:set_character_instances
      (
         UpdatedCharacterInstances,
         battle:set_current_player_turn
         (
            NextPlayerTurn,
            Battle
         )
      ),
   {ActivatedCharacterInstanceIXs, UpdatedBattle}.

-spec finalize_character_turn (query_state()) -> query_result().
finalize_character_turn (QueryState) ->
   Battle = QueryState#query_state.battle,
   CharacterInstances =
      battle:get_character_instances(Battle),

   AnActiveCharacterInstanceRemains =
      array:foldl
      (
         fun (_IX, CharacterInstance, Prev) ->
            (Prev or character_instance:get_is_active(CharacterInstance))
         end,
         false,
         CharacterInstances
      ),

   case AnActiveCharacterInstanceRemains of
      true ->
         io:format("~nThere are still active characters.~n"),
         #query_result
         {
            is_new_turn = false,
            updated_character_instance_ixs = [],
            updated_battle = Battle
         };
      false ->
         io:format("~nThere are no more active characters.~n"),
         {UpdatedCharacterInstanceIXs, UpdatedBattle} =
            start_next_players_turn(QueryState),
         #query_result
         {
            is_new_turn = true,
            updated_character_instance_ixs = UpdatedCharacterInstanceIXs,
            updated_battle = UpdatedBattle
         }
   end.

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

-spec update_cache (query_result(), input()) -> 'ok'.
update_cache (QueryResult, Input) ->
   PlayerID = Input#input.player_id,
   BattleID = Input#input.battle_id,
   Battle = QueryResult#query_result.updated_battle,

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
            list_to_binary(io_lib:format("~p", [ClientUpdate]))
         ]
      ]
   ).

-spec handle (binary()) -> binary().
handle (Req) ->
   Input = parse_input(Req),
   PlayerID = Input#input.player_id,
   PlayerSessionToken = Input#input.session_token,
   Actions = Input#input.actions,

   security:assert_identity(PlayerID, PlayerSessionToken),
   security:lock_queries(PlayerID),

   RData = fetch_relevant_data(Input),

   assert_character_instance_can_be_played(RData, Input),

   {ActionsDiffUpdate, ClientUpdate, UpdatedRData} =
      lists:foldl
      (
         fun (Action, Prev) ->
            battle_action:handle(Action, Prev)
         end,
         {[], [], RData},
         Actions
      ),

   UpdatedBattle = finalize_and_fuse_relevant_data(UpdatedRData, Input),

   {TurnDiffUpdate, FinalizedBattle} = end_of_turn:apply_to(UpdatedBattle),

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
