-module(character_turn).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-include("../../include/yaws_api.hrl").

-record
(
   input,
   {
      session_token,
      player_id,
      battlemap_id,
      instance_id,
      char_id,
      path,
      target_id
   }
).

-record
(
   query_state,
   {
      input,
      battlemap,
      battlemap_inst,
      main_char,
      main_char_inst,
      main_char_new_loc,
      target_char,
      target_char_inst
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
   PlayerID = maps:get(<<"player_id">>, JSONReqMap),
   SessionToken = maps:get(<<"session_token">>, JSONReqMap),
   Target =
      case maps:get(<<"targets_id">>, JSONReqMap) of
         [] -> <<"">>;
         [T] -> T
      end,
   #input
   {
      player_id = PlayerID,
      battlemap_id = maps:get(<<"battlemap_id">>, JSONReqMap),
      instance_id = maps:get(<<"instance_id">>, JSONReqMap),
      char_id = maps:get(<<"char_id">>, JSONReqMap),
      path = maps:get(<<"path">>, JSONReqMap),
      target_id = Target
   }.

fetch_data (Input) ->
   Battlemap =
      timed_cache:fetch
      (
         battlemap_db,
         Input#input.player_id,
         Input#input.battlemap_id
      ),
   BattlemapInst =
      timed_cache:fetch
      (
         battlemap_instance_db,
         Input#input.player_id,
         <<"0">>
      ),
   MainChar =
      timed_cache:fetch
      (
         character_db,
         Input#input.player_id,
         Input#input.char_id
      ),
   MainCharInst =
      battlemap_instance:get_char_instance
      (
         BattlemapInst,
         Input#input.char_id
      ),
   case Input#input.target_id of
      <<"">> ->
         TargetChar = nothing,
         TargetCharInst = nothing;

      TargetID ->
         TargetChar =
            timed_cache:fetch
            (
               character_db,
               Input#input.player_id,
               TargetID
            ),
         TargetCharInst =
            battlemap_instance:get_char_instance
            (
               BattlemapInst,
               TargetID
            )
   end,
   #query_state
   {
      input = Input,
      battlemap = Battlemap,
      battlemap_inst = BattlemapInst,
      main_char = MainChar,
      main_char_inst = MainCharInst,
      main_char_new_loc = nothing,
      target_char = TargetChar,
      target_char_inst = TargetCharInst
   }.

assert_main_char_can_be_used (QueryState) ->
   false = character_instance:is_dead(QueryState#query_state.main_char_inst),
   true =
      battlemap_instance:can_play_char_instance
      (
         QueryState#query_state.battlemap_inst,
         QueryState#query_state.input#input.player_id,
         QueryState#query_state.input#input.char_id
      ).

handle_main_char_movement (QueryState) ->
   {X, Y} =
      battlemap:cross
      (
         QueryState#query_state.battlemap,
         character_instance:get_location(QueryState#query_state.main_char_inst),
         statistics:get_movement_points
         (
            character_instance:get_statistics
            (
               QueryState#query_state.main_char_inst
            )
         ),
         QueryState#query_state.input#input.path,
         battlemap_instance:get_char_instances
         (
            QueryState#query_state.battlemap_inst
         )
      ),
   QueryState#query_state
   {
      battlemap_inst =
         battlemap_instance:set_char_instance
         (
            battlemap_instance:post_play_char_instance
            (
               QueryState#query_state.battlemap_inst,
               QueryState#query_state.input#input.char_id
            ),
            QueryState#query_state.input#input.char_id,
            character_instance:set_location
            (
               QueryState#query_state.main_char_inst,
               X,
               Y
            )
         ),
      main_char_new_loc = {X, Y}
   }.

handle_target (QueryState)
   when (QueryState#query_state.target_char_inst == nothing) ->
   QueryState;
handle_target (QueryState) ->
   TargetLoc =
      character_instance:get_location(QueryState#query_state.target_char_inst),
   Dist =
      battlemap:dist(QueryState#query_state.main_char_new_loc, TargetLoc),
   AtkWeapon =
      weapon:from_id
      (
         character_instance:get_active_weapon
         (
            QueryState#query_state.main_char_inst,
            QueryState#query_state.main_char
         )
      ),
   {_, AtkRange} = weapon:get_ranges(AtkWeapon),
   io:format
   (
      "~nAttack from ~p to ~p (wp: ~p, dist: ~p, range: ~p).~n",
      [
         QueryState#query_state.main_char_new_loc,
         TargetLoc,
         weapon:get_id(AtkWeapon),
         Dist,
         AtkRange
      ]
   ),
   true = (Dist =< AtkRange),
   TargetStatistics =
      character_instance:get_statistics
      (
         QueryState#query_state.target_char_inst
      ),
   {MinDamage, MaxDamage} = weapon:get_damages(AtkWeapon),
   Diff = max(1, MaxDamage - MinDamage),
   NewTargetCharInst =
      character_instance:mod_health
      (
         QueryState#query_state.target_char_inst,
         statistics:get_health(TargetStatistics),
         (MinDamage + rand:uniform(Diff) - 1)
      ),
   %% TODO: test for (and handle) riposte.
   QueryState#query_state
   {
      battlemap_inst =
         battlemap_instance:set_char_instance
         (
            QueryState#query_state.battlemap_inst,
            QueryState#query_state.input#input.target_id,
            NewTargetCharInst
         ),
      target_char_inst = NewTargetCharInst
   }.

generate_reply (QueryState) ->
   case QueryState#query_state.target_char_inst of
      nothing -> [<<"okay">>];

      CharInst ->
         add_char:generate
         (
            QueryState#query_state.target_char,
            CharInst,
            (
               battlemap_instance:can_play_char_instance
               (
                  QueryState#query_state.battlemap_inst,
                  QueryState#query_state.input#input.player_id,
                  QueryState#query_state.input#input.target_id
               )
               and
               (not character_instance:is_dead(CharInst))
            )
         )
   end.

handle (Req) ->
   %%%% Parse
   Input = parse_input(Req),
   assert_player_identity(Req),
   %%%% Fetch
   QueryState = fetch_data(Input),
   %%%% Calc
   assert_main_char_can_be_used(QueryState),
   NQueryState = handle_target(handle_main_char_movement(QueryState)),
   %%%% Commit
   database_shim:commit
   (
      battlemap_instance_db,
      Input#input.player_id,
      <<"0">>,
      NQueryState#query_state.battlemap_inst
   ),
   %%%% Reply
   jiffy:encode([generate_reply(NQueryState)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
out(A) ->
   {
      content,
      "application/json; charset=UTF-8",
      handle(A#arg.clidata)
   }.
