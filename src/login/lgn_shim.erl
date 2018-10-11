-module(lgn_shim).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([generate_random_player/4]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec add_ref_to_event
   (
      binary(),
      binary(),
      boolean(),
      shr_player:type()
   )
   -> shr_player:type().
add_ref_to_event (BattleID, EventName, IsPlayersTurn, Player) ->
   Event =
      shr_battle_summary:new(BattleID, EventName, <<"Never">>, IsPlayersTurn),

   Result = shr_player:set_event_summaries([Event], Player),

   Result.

-spec add_ref_to_map
   (
      binary(),
      binary(),
      shr_player:type()
   )
   -> shr_player:type().
add_ref_to_map (MapID, EventName, Player) ->
   MapRef = shr_map_summary:new(MapID, EventName),

   Result = shr_player:set_map_summaries([MapRef], Player),

   Result.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec generate_random_player
   (
      binary(),
      binary(),
      binary(),
      binary()
   )
   -> shr_player:type().
generate_random_player (ID, Username, Password, Email) ->
   Result = shr_player:new(ID, Username, Password, Email),

   S0Result =
      case ID of
         <<"0">> ->
            S0 = add_ref_to_event(<<"0">>, <<"Test Battle">>, true, Result),
            S1 = shr_player:set_roster_id(<<"0">>, S0),
            S2 = add_ref_to_map(<<"0">>, <<"Test Map 0">>, S1),
            S2;

         <<"1">> ->
            S0 = add_ref_to_event(<<"0">>, <<"Test Battle">>, false, Result),
            S1 = shr_player:set_roster_id(<<"1">>, S0),
            S2 = add_ref_to_map(<<"1">>, <<"Test Map 1">>, S1),

            S2;

         _ -> Result
      end,

   S0Result.
