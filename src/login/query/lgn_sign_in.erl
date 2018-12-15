-module(lgn_sign_in).

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
      player_id :: binary(),
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
      username = string:lowercase(Username),
      password = Password
   }.

-spec fetch_data (input()) -> query_state().
fetch_data (Input) ->
   Username = Input#input.username,

   % Having this be cached my be both useless and a security issue.
   PlayerID = shr_timed_cache:fetch(login_db, any, Username),

   Player = shr_timed_cache:fetch(player_db, any, PlayerID),

   #query_state
   {
      player_id = PlayerID,
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

-spec commit_update (query_state()) -> 'ok'.
commit_update (QueryState) ->
   PlayerID = QueryState#query_state.player_id,
   UpdatedPlayer = QueryState#query_state.player,
   NewToken = shr_player:get_token(UpdatedPlayer),
   NewActiveTime = shr_player:get_last_active(UpdatedPlayer),

   ok =
      ataxia_client:update
      (
         player_db,
         ataxia_security:user_from_id(PlayerID),
         ataxic:update_value
         (
            ataxic:sequence
            (
               [
                  ataxic:update_field
                  (
                     shr_player:get_token_field(),
                     ataxic:constant(NewToken)
                  ),
                  ataxic:update_field
                  (
                     shr_player:get_last_active_field(),
                     ataxic:constant(NewActiveTime)
                  )
               ]
            )
         ),
         PlayerID
      ),

   shr_timed_cache:update(player_db, any, PlayerID, UpdatedPlayer),

   'ok'.

-spec generate_reply(query_state()) -> binary().
generate_reply (QueryState) ->
   Player = QueryState#query_state.player,

   SetSession = lgn_set_session:generate(Player),
   Output = jiffy:encode([SetSession]),

   Output.

-spec handle (binary()) -> binary().
handle (Req) ->
   Input = parse_input(Req),
   QueryState = fetch_data(Input),
   Update = update_data(QueryState, Input),
   commit_update(Update),
   generate_reply(Update).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
out(A) ->
   {
      content,
      "application/json; charset=UTF-8",
      handle(A#arg.clidata)
   }.
