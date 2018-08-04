-module(plr_sign_in).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-include("../../../include/yaws_api.hrl").

-record
(
   input,
   {
      username :: binary(),
      password :: binary()
   }
).

-record
(
   query_state,
   {
      player :: shr_player:type()
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
   Username = maps:get(<<"usr">>, JSONReqMap),
   Password = maps:get(<<"pwd">>, JSONReqMap),

   #input
   {
      username = Username,
      password = Password
   }.

-spec fetch_data (input()) -> query_state().
fetch_data (Input) ->
   PlayerID = Input#input.username,

   Player = shr_timed_cache:fetch(player_db, any, PlayerID),

   #query_state
   {
      player = Player
   }.

-spec update_data (query_state(), input()) -> query_state().
update_data (QueryState, Input) ->
   InputPassword = Input#input.password,
   Player = QueryState#query_state.player,

   true = (shr_player:password_is(InputPassword, Player)),

   S0Player = shr_player:new_token(Player),
   S1Player = shr_player:refresh_active(S0Player),

   QueryState#query_state
   {
      player = S1Player
   }.

-spec commit_update (query_state(), input()) -> 'ok'.
commit_update (QueryState, Input) ->
   PlayerID = Input#input.username,
   UpdatedPlayer = QueryState#query_state.player,
   NewToken = shr_player:get_token(UpdatedPlayer),
   NewActiveTime = shr_player:get_last_active(UpdatedPlayer),

   Query =
      shr_db_query:new
      (
         player_db,
         PlayerID,
         {user, PlayerID},
         [
            shr_db_query:set_field
            (
               shr_player:get_token_field(),
               NewToken
            ),
            shr_db_query:set_field
            (
               shr_player:get_last_active_field(),
               NewActiveTime
            )
         ]
      ),

   shr_database:commit(Query),
   shr_timed_cache:update(player_db, PlayerID, PlayerID, UpdatedPlayer),

   'ok'.

-spec generate_reply(query_state()) -> binary().
generate_reply (QueryState) ->
   Player = QueryState#query_state.player,

   SetSession = plr_set_session:generate(Player),
   Output = jiffy:encode([SetSession]),

   Output.

-spec handle (binary()) -> binary().
handle (Req) ->
   Input = parse_input(Req),
   QueryState = fetch_data(Input),
   Update = update_data(QueryState, Input),
   commit_update(Update, Input),
   generate_reply(QueryState).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
out(A) ->
   {
      content,
      "application/json; charset=UTF-8",
      handle(A#arg.clidata)
   }.
