-module(character_turn).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-include("../../include/yaws_api.hrl").

-record
(
   input,
   {
      player_id,
      session_token,
      battlemap_instance_id,
      character_instance_ix,
      path,
      target_ix
   }
).

-record
(
   query_state,
   {
      battlemap_instance,
      character_instance
   }
).

-record
(
   query_result,
   {
      is_new_turn,
      updated_character_instance_ixs,
      updated_battlemap_instance
   }
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([out/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
parse_input (Req) ->
   JSONReqMap = jiffy:decode(Req, [return_maps]),
   CharacterInstanceIX = binary_to_integer(maps:get(<<"cix">>, JSONReqMap)),
   TargetIX = binary_to_integer(maps:get(<<"tix">>, JSONReqMap)),
   #input
   {
      player_id = maps:get(<<"pid">>, JSONReqMap),
      session_token = maps:get(<<"stk">>, JSONReqMap),
      battlemap_instance_id = maps:get(<<"bmi">>, JSONReqMap),
      character_instance_ix = CharacterInstanceIX,
      path = maps:get(<<"p">>, JSONReqMap),
      target_ix = TargetIX
   }.

fetch_data (Input) ->
   PlayerID = Input#input.player_id,
   BattlemapInstanceID = Input#input.battlemap_instance_id,
   CharacterInstanceIX = Input#input.character_instance_ix,

   BattlemapInstance =
      timed_cache:fetch
      (
         battlemap_instance_db,
         PlayerID,
         BattlemapInstanceID
      ),
   CharacterInstance =
      array:get
      (
         CharacterInstanceIX,
         battlemap_instance:get_character_instances(BattlemapInstance)
      ),

   #query_state
   {
      battlemap_instance = BattlemapInstance,
      character_instance = CharacterInstance
   }.

assert_character_instance_can_be_played (QueryState, Input) ->
   %%% Var
   BattlemapInstance = QueryState#query_state.battlemap_instance,
   PlayerID = Input#input.player_id,
   ControlledCharacterInstance = QueryState#query_state.character_instance,
   %%% Asserts
   PlayerID =
      array:get
      (
         player_turn:get_player_ix
         (
            battlemap_instance:get_current_player_turn(BattlemapInstance)
         ),
         battlemap_instance:get_player_ids(BattlemapInstance)
      ),
   PlayerID =
      character:get_owner_id
      (
         character_instance:get_character(ControlledCharacterInstance)
      ),
   true = character_instance:get_is_active(ControlledCharacterInstance).

handle_character_instance_moving (QueryState, Input) ->
   BattlemapInstance = QueryState#query_state.battlemap_instance,
   ControlledCharacterInstance = QueryState#query_state.character_instance,
   ControlledCharacter =
      character_instance:get_character(ControlledCharacterInstance),
   ControlledCharacterIX = Input#input.character_instance_ix,

   Path = Input#input.path,
   Battlemap = battlemap_instance:get_battlemap(BattlemapInstance),
   ControlledCharacterStatistics =
      character:get_statistics(ControlledCharacter),
   ControlledCharacterMovementPoints =
      statistics:get_movement_points(ControlledCharacterStatistics),

   ForbiddenLocations =
      array:map
      (
         fun (_IX, CharacterInstance) ->
            character_instance:get_location(CharacterInstance)
         end,
         battlemap_instance:get_character_instances(BattlemapInstance)
      ),
   {NewLocation, Cost} =
      movement:cross
      (
         Battlemap,
         ForbiddenLocations,
         Path,
         character_instance:get_location(ControlledCharacterInstance)
      ),

   io:format
   (
      "~nMoving from ~p to ~p (cost ~p) with ~p movement points.~n",
      [
         character_instance:get_location(ControlledCharacterInstance),
         NewLocation,
         Cost,
         ControlledCharacterMovementPoints
      ]
   ),

   true = (Cost =< ControlledCharacterMovementPoints),

   UpdatedQueryState =
      QueryState#query_state
      {
         character_instance =
            character_instance:set_location
            (
               NewLocation,
               ControlledCharacterInstance
            )
      },
   {
      [{move, ControlledCharacterIX, NewLocation}],
      UpdatedQueryState
   }.

handle_character_instance_switching_weapons (QueryState, Input) ->
   ControlledCharacterInstance = QueryState#query_state.character_instance,
   ControlledCharacter =
      character_instance:get_character(ControlledCharacterInstance),
   ControlledCharacterAttributes =
      character:get_attributes(ControlledCharacter),
   {PrimaryWeapon, SecondaryWeapon} =
      character:get_weapons(ControlledCharacter),
   ControlledCharacterIX = Input#input.character_instance_ix,

   UpdatedWeapons = {SecondaryWeapon, PrimaryWeapon},
   UpdatedControlledCharacterStatistics =
      statistics:new
      (
         ControlledCharacterAttributes,
         UpdatedWeapons
      ),
   UpdatedControlledCharacter =
      character:set_statistics
      (
         UpdatedControlledCharacterStatistics,
         character:set_weapons
         (
            ControlledCharacter
         )
      ),
   UpdatedControlledCharacterInstance =
      character_instance:set_character
      (
         UpdatedControlledCharacter,
         ControlledCharacterInstance
      ),
   UpdatedQueryState =
      QueryState#query_state
      {
         character_instance = UpdatedControlledCharacterInstance
      },

   {
      [{switch_weapons, ControlledCharacterIX}],
      UpdatedQueryState
   }.

-include("character_turn/handle_character_instance_attacking_2.erl").

get_type_of_turn (Input) ->
   case {Input#input.path, Input#input.target_ix} of
      {[], -1} -> [nothing, nothing];
      {[], _} -> [nothing, attack];
      {[<<"S">>], -1} -> [switch, nothing];
      {[<<"S">>], _} -> [switch, attack];
      {_, -1} -> [move, nothing];
      {_, _} -> [move, attack]
   end.

finalize_character_instance (QueryState, Input) ->
   BattlemapInstance = QueryState#query_state.battlemap_instance,
   FinalizedCharacterInstance =
      character_instance:set_is_active
      (
         false,
         QueryState#query_state.character_instance
      ),
   QueryState#query_state
   {
      battlemap_instance =
         battlemap_instance:set_character_instances
         (
            array:set
            (
               Input#input.character_instance_ix,
               FinalizedCharacterInstance,
               battlemap_instance:get_character_instances(BattlemapInstance)
            ),
            BattlemapInstance
         ),
      character_instance = FinalizedCharacterInstance
   }.

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

start_next_players_turn (QueryState) ->
   BattlemapInstance = QueryState#query_state.battlemap_instance,
   PlayerIDs = battlemap_instance:get_player_ids(BattlemapInstance),
   PlayerTurn = battlemap_instance:get_current_player_turn(BattlemapInstance),
   CurrentPlayerIX = player_turn:get_player_ix(PlayerTurn),
   CurrentTurnNumber = player_turn:get_number(PlayerTurn),
   CharacterInstances =
      battlemap_instance:get_character_instances(BattlemapInstance),

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
         array:size(CharacterInstances)
      ),
   UpdatedBattlemapInstance =
      battlemap_instance:set_character_instances
      (
         UpdatedCharacterInstances,
         battlemap_instance:set_player_turn
         (
            NextPlayerTurn,
            BattlemapInstance
         )
      ),
   {ActivatedCharacterInstanceIXs, UpdatedBattlemapInstance}.

finalize_character_turn (QueryState) ->
   BattlemapInstance = QueryState#query_state.battlemap_instance,
   CharacterInstances =
      battlemap_instance:get_character_instances(BattlemapInstance),

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
         #query_result
         {
            is_new_turn = false,
            updated_character_instance_ixs = [],
            updated_battlemap_instance = BattlemapInstance
         };
      false ->
         {UpdatedCharacterInstanceIXs, UpdatedBattlemapInstance} =
            start_next_players_turn(QueryState),
         #query_result
         {
            is_new_turn = true,
            updated_character_instance_ixs = UpdatedCharacterInstanceIXs,
            updated_battlemap_instance = UpdatedBattlemapInstance
         }
   end.

play (DiffUpdate, QueryState, [], _Input) ->
   {DiffUpdate, QueryState};
play (DiffUpdate, QueryState, [nothing|Next], Input) ->
   play(DiffUpdate, QueryState, Next, Input);
play (DiffUpdate, QueryState, [move|Next], Input) ->
   {AddedDiffContent, NewQueryState} =
      handle_character_instance_moving(QueryState, Input),
   play
   (
      (AddedDiffContent ++ DiffUpdate),
      NewQueryState,
      Next,
      Input
   );
play (DiffUpdate, QueryState, [switch|Next], Input) ->
   {AddedDiffContent, NewQueryState} =
      handle_character_instance_switching_weapons(QueryState, Input),
   play
   (
      (AddedDiffContent ++ DiffUpdate),
      NewQueryState,
      Next,
      Input
   );

play (DiffUpdate, QueryState, [attack|Next], Input) ->
   {AddedDiffContent, NewQueryState} =
      handle_character_instance_attacking(QueryState, Input),
   play
   (
      (AddedDiffContent ++ DiffUpdate),
      NewQueryState,
      Next,
      Input
   ).

send_to_database (QueryResult, _TurnType, Input) ->
   PlayerID = Input#input.player_id,
   BattlemapInstanceID = Input#input.battlemap_instance_id,
   BattlemapInstance = QueryResult#query_result.updated_battlemap_instance,

   %% TODO: differential commit
   database_shim:commit
   (
      battlemap_instance_db,
      PlayerID,
      BattlemapInstanceID,
      BattlemapInstance
   ).

update_cache (QueryResult, Input) ->
   PlayerID = Input#input.player_id,
   BattlemapInstanceID = Input#input.battlemap_instance_id,
   BattlemapInstance = QueryResult#query_result.updated_battlemap_instance,

   timed_cache:update
   (
      battlemap_instance_db,
      PlayerID,
      BattlemapInstanceID,
      BattlemapInstance
   ).

generate_reply (_QueryResult, DiffUpdate, _TurnType, _Input) ->
   %% TODO
   jiffy:encode
   (
      [
         [
            <<"raw">>,
            list_to_binary(io_lib:format("~p", [DiffUpdate]))
         ]
      ]
   ).

handle (Req) ->
   Input = parse_input(Req),
   security:assert_identity(Input#input.player_id, Input#input.session_token),
   security:lock_queries(Input#input.player_id),
   QueryState = fetch_data(Input),
   assert_character_instance_can_be_played(QueryState, Input),
   TurnType = get_type_of_turn(Input),
   {DiffUpdate, UpdatedQueryState} = play([], QueryState, TurnType, Input),
   QueryResult =
      finalize_character_turn
      (
         finalize_character_instance(UpdatedQueryState, Input)
      ),
   send_to_database(QueryResult, TurnType, Input),
   update_cache(QueryResult, Input),
   io:format("~nCharacter turn result:~n~p~n", [DiffUpdate]),
   security:unlock_queries(Input#input.player_id),
   generate_reply(QueryResult, DiffUpdate, TurnType, Input).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
out(A) ->
   {
      content,
      "application/json; charset=UTF-8",
      handle(A#arg.clidata)
   }.
