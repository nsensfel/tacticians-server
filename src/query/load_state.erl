-module(load_state).

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
      instance_id
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
   SessionToken =  maps:get(<<"session_token">>, JSONReqMap),
   database_shim:assert_session_is_valid(PlayerID, SessionToken),
   #input
   {
      player_id = PlayerID,
      battlemap_id = maps:get(<<"battlemap_id">>, JSONReqMap),
      instance_id = maps:get(<<"instance_id">>, JSONReqMap)
   }.

generate_set_map (Battlemap) ->
   jiffy:encode
   (
      {
         [
            {<<"width">>, battlemap:get_width(Battlemap)},
            {<<"height">>, battlemap:get_height(Battlemap)},
            {<<"content">>, battlemap:list_tiles(Battlemap)}
         ]
      }
   ).

generate_add_char (Char, CharInstance, BattlemapInstance, PlayerID) ->
   {X, Y} = character_instance:get_location(CharInstance),
   CharID = character:get_id(Char),
   jiffy:encode
   (
      {
         [
            {<<"id">>, character:get_id(Char)},
            {<<"name">>, character:get_name(Char)},
            {<<"icon">>, character:get_icon(Char)},
            {<<"portrait">>, character:get_portrait(Char)},
            {<<"health">>, character_instance:get_current_health(CharInstance)},
            {<<"max_health">>, character:get_max_health(Char)},
            {<<"loc_x">>, X},
            {<<"loc_y">>, Y},
            {<<"team">>, character_instance:get_owner(CharInstance)},
            {<<"mov_pts">>, character:get_movement_points(Char)},
            {<<"atk_rg">>, character:get_attack_range(Char)},
            {
               <<"enabled">>,
               battlemap_instance:can_play_char_instance
               (
                  BattlemapInstance,
                  PlayerID,
                  CharID
               )
            }
         ]
      }
   ).

generate_reply (Battlemap, BattlemapInstance, Characters, PlayerID) ->
   jiffy:encode
   (
      [
         [
            <<"set_map">>,
            generate_set_map(Battlemap)
         ]
         |
         lists:map
         (
            fun ({CharID, CharInstance}) ->
               [
                  <<"add_char">>,
                  generate_add_char
                  (
                     CharID,
                     CharInstance,
                     BattlemapInstance,
                     PlayerID
                  )
               ]
            end,
            Characters
         )
      ]
   ).

handle (Req) ->
   %%%% Parse
   Input = parse_input(Req),
   %%%% Fetch
   Battlemap =
      timed_cache:fetch
      (
         battlemap_db,
         Input#input.player_id,
         Input#input.battlemap_id
      ),
   BattlemapInstance =
      timed_cache:fetch
      (
         battlemap_instance_db,
         Input#input.player_id,
         <<"0">>
      ),
   Characters =
      lists:map
      (
         fun ({CharID, CharInst}) ->
            {
               timed_cache:fetch(character_db, Input#input.player_id, CharID),
               CharInst
            }
         end,
         battlemap_instance:list_characters(BattlemapInstance)
      ),
   %%%% Calc
   %%%% Commit
   %%%% Reply
   generate_reply
   (
      Battlemap,
      BattlemapInstance,
      Characters,
      Input#input.player_id
   ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
out(A) ->
   {
      content,
      "application/json; charset=UTF-8",
      handle(A#arg.clidata)
   }.
