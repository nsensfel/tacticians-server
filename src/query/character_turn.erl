-module(character_turn).

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
      battlemap_instance_id :: binary(),
      character_instance_ix :: non_neg_integer(),
      path :: list(binary()),
      target_ix :: (-1 | non_neg_integer())
   }
).

-record
(
   query_state,
   {
      battlemap_instance :: battlemap_instance:struct(),
      character_instance :: character_instance:struct()
   }
).

-record
(
   query_result,
   {
      is_new_turn :: boolean(),
      updated_character_instance_ixs :: list(non_neg_integer()),
      updated_battlemap_instance :: battlemap_instance:struct()
   }
).

-type input() :: #input{}.
-type query_state() :: #query_state{}.
-type query_result() :: #query_result{}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([out/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec parse_input (binary()) -> input().
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

-spec fetch_data (input()) -> query_state().
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

-spec assert_character_instance_can_be_played
   (
      query_state(),
      input()
   )
   -> 'ok'.
assert_character_instance_can_be_played (QueryState, Input) ->
   BattlemapInstance = QueryState#query_state.battlemap_instance,
   PlayerID = Input#input.player_id,
   ControlledCharacterInstance = QueryState#query_state.character_instance,

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
   true = character_instance:get_is_active(ControlledCharacterInstance),

   ok.

-spec handle_character_instance_moving
   (
      query_state(),
      input()
   )
   -> {list(any()), query_state()}.
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

-spec handle_character_instance_switching_weapons
   (
      query_state(),
      input()
   )
   -> {list(any()), query_state()}.
handle_character_instance_switching_weapons (QueryState, Input) ->
   ControlledCharacterInstance = QueryState#query_state.character_instance,
   ControlledCharacter =
      character_instance:get_character(ControlledCharacterInstance),
   ControlledCharacterAttributes =
      character:get_attributes(ControlledCharacter),
   {PrimaryWeapon, SecondaryWeapon} =
      character:get_weapon_ids(ControlledCharacter),
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
         character:set_weapon_ids
         (
            UpdatedWeapons,
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

-spec set_new_healths_in_query_state
   (
      non_neg_integer(),
      non_neg_integer(),
      query_state(),
      input()
   )
   -> query_state().
set_new_healths_in_query_state
(
   RemainingAttackerHealth,
   RemainingDefenderHealth,
   QueryState,
   Input
) ->
   BattlemapInstance = QueryState#query_state.battlemap_instance,
   ControlledCharacterInstance = QueryState#query_state.character_instance,
   CharacterInstances =
      battlemap_instance:get_character_instances(BattlemapInstance),
   TargettedCharacterInstanceIX = Input#input.target_ix,
   TargettedCharacterInstance =
      array:get
      (
         TargettedCharacterInstanceIX,
         CharacterInstances
      ),

   QueryState#query_state
   {
      battlemap_instance =
         battlemap_instance:set_character_instances
         (
            array:set
            (
               TargettedCharacterInstanceIX,
               character_instance:set_current_health
               (
                  RemainingDefenderHealth,
                  TargettedCharacterInstance
               ),
               CharacterInstances
            ),
            BattlemapInstance
         ),
      character_instance =
         character_instance:set_current_health
         (
            RemainingAttackerHealth,
            ControlledCharacterInstance
         )
   }.

-spec handle_character_instance_attacking
   (
      query_state(),
      input()
   )
   -> {list(attack:attack_desc()), query_state()}.
handle_character_instance_attacking (QueryState, Input) ->
   BattlemapInstance = QueryState#query_state.battlemap_instance,
   ControlledCharacterInstance = QueryState#query_state.character_instance,
   ControlledCharacter =
      character_instance:get_character(ControlledCharacterInstance),
   ControlledCharacterStatistics =
      character:get_statistics(ControlledCharacter),
   TargettedCharacterInstance =
      array:get
      (
         Input#input.target_ix,
         battlemap_instance:get_character_instances(BattlemapInstance)
      ),
   TargettedCharacter =
      character_instance:get_character(TargettedCharacterInstance),
   TargettedCharacterStatistics = character:get_statistics(TargettedCharacter),
   RequiredRange =
      movement:steps_between
      (
         character_instance:get_location(ControlledCharacterInstance),
         character_instance:get_location(TargettedCharacterInstance)
      ),
   {AttackingWeaponID, _} = character:get_weapon_ids(ControlledCharacter),
   AttackingWeapon = weapon:from_id(AttackingWeaponID),
   {DefendingWeaponID, _} = character:get_weapon_ids(TargettedCharacter),
   DefendingWeapon = weapon:from_id(DefendingWeaponID),
   BaseAttackerHealth =
      character_instance:get_current_health(ControlledCharacterInstance),
   BaseDefenderHealth =
      character_instance:get_current_health(TargettedCharacterInstance),

   AttackSequence =
      attack:get_sequence(RequiredRange, AttackingWeapon, DefendingWeapon),

   AttackEffects =
      lists:map
      (
         fun (AttackOrder) ->
            attack:get_description_of
            (
               AttackOrder,
               ControlledCharacterStatistics,
               TargettedCharacterStatistics
            )
         end,
         AttackSequence
      ),

   {AttackSummary, RemainingAttackerHealth, RemainingDefenderHealth} =
      lists:foldl
      (
         fun
         (
            AttackEffect,
            {
               CurrentAttackEffects,
               CurrentAttackerHealth,
               CurrentDefenderHealth
            }
         ) ->
            {AttackTrueEffect, NewAttackerHealth, NewDefenderHealth} =
               attack:apply_to_healths
               (
                  AttackEffect,
                  CurrentAttackerHealth,
                  CurrentDefenderHealth
               ),
            {
               [AttackTrueEffect|CurrentAttackEffects],
               NewAttackerHealth,
               NewDefenderHealth
            }
         end,
         {[], BaseAttackerHealth, BaseDefenderHealth},
         AttackEffects
      ),

   {
      AttackSummary,
      set_new_healths_in_query_state
      (
         RemainingAttackerHealth,
         RemainingDefenderHealth,
         QueryState,
         Input
      )
   }.

-spec get_type_of_turn (input()) -> list(atom()).
get_type_of_turn (Input) ->
   case {Input#input.path, Input#input.target_ix} of
      {[], -1} -> [nothing, nothing];
      {[], _} -> [nothing, attack];
      {[<<"S">>], -1} -> [switch, nothing];
      {[<<"S">>], _} -> [switch, attack];
      {_, -1} -> [move, nothing];
      {_, _} -> [move, attack]
   end.

-spec finalize_character_instance
   (
      query_state(),
      input()
   )
   -> query_state().
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
   -> {list(non_neg_integer()), battlemap_instance:struct()}.
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
         (array:size(CharacterInstances) - 1)
      ),
   UpdatedBattlemapInstance =
      battlemap_instance:set_character_instances
      (
         UpdatedCharacterInstances,
         battlemap_instance:set_current_player_turn
         (
            NextPlayerTurn,
            BattlemapInstance
         )
      ),
   {ActivatedCharacterInstanceIXs, UpdatedBattlemapInstance}.

-spec finalize_character_turn (query_state()) -> query_result().
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
         io:format("~nThere are still active characters.~n"),
         #query_result
         {
            is_new_turn = false,
            updated_character_instance_ixs = [],
            updated_battlemap_instance = BattlemapInstance
         };
      false ->
         io:format("~nThere are no more active characters.~n"),
         {UpdatedCharacterInstanceIXs, UpdatedBattlemapInstance} =
            start_next_players_turn(QueryState),
         #query_result
         {
            is_new_turn = true,
            updated_character_instance_ixs = UpdatedCharacterInstanceIXs,
            updated_battlemap_instance = UpdatedBattlemapInstance
         }
   end.

-spec play
   (
      list(any()),
      query_state(),
      list(atom()),
      input()
   )
   -> {list(any()), query_state()}.
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

-spec send_to_database (query_result(), any(), input()) -> 'ok'.
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

-spec update_cache (query_result(), input()) -> 'ok'.
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

-spec generate_reply
   (
      query_result(),
      list(any()),
      any(),
      input()
   )
   -> binary().
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

-spec handle (binary()) -> binary().
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
