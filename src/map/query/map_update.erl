-module(map_update).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-include("../../../include/yaws_api.hrl").

-record
(
   input,
   {
      player_id :: binary(),
      session_token :: binary(),
      map_id :: binary(),
      w :: non_neg_integer(),
      h :: non_neg_integer(),
      t :: list(list(non_neg_integer()))
   }
).

-record
(
   query_state,
   {
      map :: map_map:type()
   }
).

-type input() :: #input{}.
-type query_state() :: #query_state{}.

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
   PlayerID = maps:get(<<"pid">>, JSONReqMap),
   SessionToken =  maps:get(<<"stk">>, JSONReqMap),
   MapID = maps:get(<<"mid">>, JSONReqMap),
   MapWidth = maps:get(<<"w">>, JSONReqMap),
   MapHeight = maps:get(<<"h">>, JSONReqMap),
   MapContent = maps:get(<<"t">>, JSONReqMap),

   %% TODO [LOW]: those checks should be done while queries are locked.
   true = (MapWidth > 0),
   true = (MapHeight > 0),
   true = (length(MapContent) == (MapWidth * MapHeight)),
   true =
      lists:all
      (
         fun (T) ->
            [M|[V|B]] = T,
            (
               (M > 0)
               and (V >= 0)
               and ((length(B) rem 2) == 0)
               and
               lists:all
               (
                  fun (Bo) ->
                     %% FIXME [SECURITY][LOW]: this does not prevent "Error"
                     %% tiles.
                     (Bo >= 0)
                  end,
                  B
               )
            )
         end,
         MapContent
      ),

   #input
   {
      player_id = PlayerID,
      session_token = SessionToken,
      map_id = MapID,
      w = MapWidth,
      h = MapHeight,
      t = MapContent
   }.

-spec authenticate_user (input()) -> ('ok' | 'error').
authenticate_user (Input) ->
   PlayerID = Input#input.player_id,
   SessionToken = Input#input.session_token,

   Player = shr_timed_cache:fetch(player_db, any, PlayerID),

   case shr_security:credentials_match(SessionToken, Player) of
      true -> ok;
      _ -> error
   end.

-spec fetch_data (input()) -> query_state().
fetch_data (Input) ->
   PlayerID = Input#input.player_id,
   MapID = Input#input.map_id,

   Map = shr_timed_cache:fetch(map_db, PlayerID, MapID),

   #query_state
   {
      map = Map
   }.

-spec update_data (query_state(), input()) -> query_state().
update_data (QueryState, Input) ->
   QueryState#query_state
   {
      map =
         map_map:update_from_list
         (
            QueryState#query_state.map,
            Input#input.w,
            Input#input.h,
            Input#input.t
         )
   }.

-spec commit_update (query_state(), input()) -> 'ok'.
commit_update (QueryState, Input) ->
   PlayerID = Input#input.player_id,
   MapID = Input#input.map_id,
   Map = QueryState#query_state.map,

   Query =
      shr_db_query:new
      (
         map_db,
         MapID,
         {user, PlayerID},
         [
            shr_db_query:set_field
            (
               map_map:get_height_field(),
               Input#input.h
            ),
            shr_db_query:set_field
            (
               map_map:get_width_field(),
               Input#input.w
            ),
            shr_db_query:set_field
            (
               map_map:get_tile_instances_field(),
               map_map:get_tile_instances(Map)
            )
         ]
      ),

   shr_database:commit(Query),
   shr_timed_cache:update(map_db, PlayerID, MapID, Map),

   'ok'.

-spec generate_reply () -> binary().
generate_reply () ->
   jiffy:encode([shr_okay:generate()]).

-spec handle (binary()) -> binary().
handle (Req) ->
   Input = parse_input(Req),
   case authenticate_user(Input) of
      ok ->
         shr_security:lock_queries(Input#input.player_id),
         QueryState = fetch_data(Input),
         Update = update_data(QueryState, Input),
         commit_update(Update, Input),
         shr_security:unlock_queries(Input#input.player_id),
         generate_reply();

      error -> jiffy:encode([shr_disconnected:generate()])
   end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
out(A) ->
   {
      content,
      "application/json; charset=UTF-8",
      handle(A#arg.clidata)
   }.
