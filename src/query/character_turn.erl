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
      character_instance_id,
      path,
      target_id
   }
).

-record
(
   query_state,
   {
      battlemap_instance,
      character_instance,
      rolls
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
   CharacterInstanceID = binary_to_integer(maps:get(<<"cid">>, JSONReqMap)),
   TargetID = binary_to_integer(maps:get(<<"tid">>, JSONReqMap)),
   #input
   {
      player_id = maps:get(<<"pid">>, JSONReqMap),
      session_token = maps:get(<<"stk">>, JSONReqMap),
      battlemap_instance_id = maps:get(<<"bmp">>, JSONReqMap),
      character_instance_id = CharacterInstanceID,
      path = maps:get(<<"p">>, JSONReqMap),
      target_id = TargetID
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
         Input#input.character_instance_id,
         battlemap_instance:get_character_instances(BattlemapInstance)
      ),
   #query_state
   {
      battlemap_instance = BattlemapInstance,
      character_instance = CharacterInstance,
      rolls = []
   }.

assert_character_can_be_played (QueryState, Input) ->
   %%% Var
   BattlemapInstance = QueryState#query_state.battlemap_instance,
   PlayerID = Input#input.player_id,
   ControlledCharacterInstance = QueryState#query_state.character_instance,
   %%% Asserts
   PlayerID =
      arrays:get
      (
         player_turn:get_player_id
         (
            battlemap_instance:get_player_turn(BattlemapInstance)
         ),
         battlemap_instance:get_players(BattlemapInstance)
      ),
   PlayerID =
      character:get_owner
      (
         character_instance:get_character(ControlledCharacterInstance)
      ),
   true = character_instance:is_active(ControlledCharacterInstance).

handle_character_moving (QueryState, Input) ->
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

handle_character_switching_weapons (QueryState) ->
   ControlledCharacterInstance = QueryState#query_state.character_instance,
   ControlledCharacter =
      character_instance:get_character(ControlledCharacterInstance),
   QueryState#query_state
   {
      character_instance =
         character_instance:set_character
         (
            character:set_weapons
            (
               weapon_set:switch
               (
                  character:get_weapons(ControlledCharacter)
               ),
               ControlledCharacter
            ),
            ControlledCharacterInstance
         )
   }.

-include("character_turn/handle_character_attacking_2.erl").

get_type_of_turn (Input) ->
   case {Input#input.path, Input#input.target_id} of
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
         battlemap_instance:set_characters
         (
            array:set
            (
               Input#input.character_instance_id,
               FinalizedCharacterInstance,
               battlemap_instance:get_characters(BattlemapInstance)
            ),
            BattlemapInstance
         ),
      character_instance = FinalizedCharacterInstance
   }.

unnamed_function (IDs, CharacterInstances, _Owner, -1) ->
   {IDs, CharacterInstances};
unnamed_function (IDs, CharacterInstances, Owner, I) ->
   CharacterInstance = array:get(I, CharacterInstances),
   Character = character_instance:get_character(CharacterInstance),
   case character:get_owner(Character) of
      OwnerID when (OwnerID == Owner) ->
         unnamed_function
         (
            [I|IDs],
            array:set
            (
               I,
               character_instance:set_is_active(true, CharacterInstance),
               CharacterInstances
            ),
            Owner,
            (I - 1)
         );

      _ ->
         unnamed_function
         (
            IDs,
            CharacterInstances,
            Owner,
            (I - 1)
         )
   end.

start_next_players_turn (QueryState, Input) ->
   BattlemapInstance = QueryState#query_state.battlemap_instance,
   Players = battlemap_instance:get_players(BattlemapInstance),
   PlayerTurn = battlemap_instance:get_player_turn(BattlemapInstance),
   CurrentPlayerID = player_turn:get_player_id(PlayerTurn),
   CurrentTurnNumber = player_turn:get_number(PlayerTurn),
   CharacterInstances =
      battlemap_instance:get_character_instances(BattlemapInstance),

   NextPlayer = ((CurrentPlayerID + 1) rem (array:size(Players))),
   NextPlayerTurn =
      player_turn:new
      (
         case NextPlayer of
            0 -> (CurrentTurnNumber + 1);
            _ -> CurrentTurnNumber
         end,
         NextPlayer
      ),

   {ActivatedCharacterInstancesID, UpdatedCharacterInstances} =
      unnamed_function
      (
         [],
         CharacterInstances,
         array:get(NextPlayer, Players),
         array:size(CharacterInstances)
      ),
   {
      ActivatedCharacterInstancesID,
      QueryState#query_state
      {
         battlemap_instance =
            battlemap_instance:set_character_instances
            (
               UpdatedCharacterInstances,
               battlemap_instance:set_player_turn
               (
                  NextPlayerTurn,
                  BattlemapInstance
               )
            )
      }
   }.

finalize_character_turn (QueryState, Input) ->
   BattlemapInstance = QueryState#query_state.battlemap_instance,
   Players = battlemap_instance:get_players(BattlemapInstance),

   CharacterInstanceRemains =
      array:foldl
      (
         fun (_I, CharacterInstance, Prev) ->
            (Prev or character_instance:get_is_active(CharacterInstance))
         end,
         Players
      ),

   case CharacterInstanceRemains of
      true -> {false, {[], QueryState}};
      false -> {true, start_next_players_turn(QueryState, Input)}
   end.

play (QueryState, [], Input) ->
   finalize_character_turn
   (
      finalize_character_instance(QueryState, Input),
      Input
   );
play (QueryState, [nothing|Next], Input) ->
   play(QueryState, Next, Input);
play (QueryState, [move|Next], Input) ->
   play
   (
      handle_character_moving(QueryState, Input),
      Next,
      Input
   );
play (QueryState, [switch|Next], Input) ->
   play
   (
      handle_character_switching_weapons(QueryState),
      Next,
      Input
   );

play (QueryState, [attack|Next], Input) ->
   play
   (
      handle_character_attacking(QueryState, Input),
      Next,
      Input
   ).

send_to_database (QueryState, TurnType, Input) ->
   unimplemented.

update_cache (QueryState, TurnType, Input) ->
   unimplemented.

generate_reply (QueryState, TurnType, Input) ->
   jiffy:encode([[<<"ok">>]]).

handle (Req) ->
   Input = parse_input(Req),
   security:assert_identity(Req#input.player_id, Req#input.session_token),
   QueryState = fetch_data(Input),
   assert_character_can_be_played(Input, QueryState),
   TurnType = get_type_of_turn(Input),
   {IsNewTurn, {UpdatedCharacterInstancesID, PostPlayQueryState}} =
      play(QueryState, TurnType, Input),
   send_to_database(PostPlayQueryState, TurnType, Input),
   update_cache(PostPlayQueryState, TurnType, Input),
   generate_reply(PostPlayQueryState, TurnType, Input).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
out(A) ->
   {
      content,
      "application/json; charset=UTF-8",
      handle(A#arg.clidata)
   }.
