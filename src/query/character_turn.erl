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
      battlemap_instance_id = maps:get(<<"bmp">>, JSONReqMap),
      character_instance_ix = CharacterInstanceIX,
      path = maps:get(<<"p">>, JSONReqMap),
      target_ix = TargetIX
   }.

fetch_data (Input) ->
   BattlemapInstance =
      timed_cache:fetch
      (
         battlemap_instance_db,
         Input#input.player_id,
         Input#input.battlemap_instance_id
      ),
   CharacterInstance =
      array:get
      (
         Input#input.character_instance_ix,
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
      arrays:get
      (
         player_turn:get_player_ix
         (
            battlemap_instance:get_player_turn(BattlemapInstance)
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
   Path = Input#input.path,
   Battlemap = battlemap_instance:get_battlemap(BattlemapInstance),
   ForbiddenLocations =
      array:map
      (
         fun (CharacterInstance) ->
            character_instance:get_location(CharacterInstance)
         end,
         battlemap_instance:get_character_instances(BattlemapInstance)
      ),
   {ok, NewLocation, _} =
      movement:cross
      (
         Battlemap,
         character_instance:get_location(ControlledCharacterInstance),
         statistics:get_movement_points
         (
            character:get_statistics(ControlledCharacter)
         ),
         Path,
         ForbiddenLocations
      ),
   QueryState#query_state
   {
      character_instance =
         character_instance:set_location
         (
            NewLocation,
            ControlledCharacterInstance
         )
   }.

handle_character_instance_switching_weapons (QueryState) ->
   ControlledCharacterInstance = QueryState#query_state.character_instance,
   ControlledCharacter =
      character_instance:get_character(ControlledCharacterInstance),
   ControlledCharacterAttributes =
      character:get_attributes(ControlledCharacter),
   {PrimaryWeapon, SecondaryWeapon} =
      character:get_weapons(ControlledCharacter),

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

   QueryState#query_state
   {
      character_instance = UpdatedControlledCharacterInstance
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
      character_instance:set_active
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

start_next_players_turn (QueryState, Input) ->
   BattlemapInstance = QueryState#query_state.battlemap_instance,
   PlayerIDs = battlemap_instance:get_player_ids(BattlemapInstance),
   PlayerTurn = battlemap_instance:get_player_turn(BattlemapInstance),
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

finalize_character_turn (QueryState, Input) ->
   BattlemapInstance = QueryState#query_state.battlemap_instance,
   CharacterInstances =
      battlemap_instance:get_character_instances(BattlemapInstance),

   AnActiveCharacterInstanceRemains =
      array:foldl
      (
         fun (_IX, CharacterInstance, Prev) ->
            (Prev or character_instance:get_is_active(CharacterInstance))
         end,
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
            start_next_players_turn(QueryState, Input),
         #query_result
         {
            is_new_turn = true,
            updated_character_instance_ixs = UpdatedCharacterInstanceIXs,
            updated_battlemap_instance = UpdatedBattlemapInstance
         }
   end.

play (QueryState, [], _Input) ->
   QueryState;
play (QueryState, [nothing|Next], Input) ->
   play(QueryState, Next, Input);
play (QueryState, [move|Next], Input) ->
   play
   (
      handle_character_instance_moving(QueryState, Input),
      Next,
      Input
   );
play (QueryState, [switch|Next], Input) ->
   play
   (
      handle_character_instance_switching_weapons(QueryState),
      Next,
      Input
   );

play (QueryState, [attack|Next], Input) ->
   play
   (
      handle_character_instance_attacking(QueryState, Input),
      Next,
      Input
   ).

send_to_database (QueryResult, TurnType, Input) ->
   unimplemented.

update_cache (QueryResult, TurnType, Input) ->
   unimplemented.

generate_reply (QueryResult, TurnType, Input) ->
   jiffy:encode([[<<"ok">>]]).

handle (Req) ->
   Input = parse_input(Req),
   security:assert_identity(Input#input.player_id, Input#input.session_token),
   security:lock_queries(Input#input.player_id),
   QueryState = fetch_data(Input),
   assert_character_instance_can_be_played(Input, QueryState),
   TurnType = get_type_of_turn(Input),
   QueryResult =
      finalize_character_turn
      (
         finalize_character_instance
         (
            play(QueryState, TurnType, Input),
            Input
         ),
         Input
      ),
   send_to_database(QueryResult, TurnType, Input),
   update_cache(QueryResult, TurnType, Input),
   security:unlock_queries(Input#input.player_id),
   generate_reply(QueryResult, TurnType, Input).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
out(A) ->
   {
      content,
      "application/json; charset=UTF-8",
      handle(A#arg.clidata)
   }.
