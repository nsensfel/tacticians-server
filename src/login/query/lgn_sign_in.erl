-module(lgn_sign_in).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
      player_id :: shr_player:id(),
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
-spec parse_input (shr_query:type()) -> input().
parse_input (Query) ->
   JSONReqMap = shr_query:get_params(Query),
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
   PlayerID = shr_timed_cache:fetch(login_db, ataxia_security:any(), Username),

   Player = shr_timed_cache:fetch(player_db, ataxia_security:any(), PlayerID),

   #query_state
   {
      player_id = PlayerID,
      player = Player
   }.

-spec update_data (query_state(), input()) -> query_state().
update_data (QueryState, Input) ->
   InputPassword = Input#input.password,
   Player = QueryState#query_state.player,

   case shr_player:password_is(InputPassword, Player) of
      false -> error({password, QueryState#query_state.player_id});
      _ -> ok
   end,

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

   shr_timed_cache:update
   (
      player_db,
      ataxia_security:any(),
      PlayerID,
      UpdatedPlayer
   ),

   'ok'.

-spec generate_reply(query_state()) -> binary().
generate_reply (QueryState) ->
   Player = QueryState#query_state.player,
   PlayerID = QueryState#query_state.player_id,

   SetSession = lgn_set_session:generate(PlayerID, Player),
   Output = jiffy:encode([SetSession]),

   Output.

-spec handle (shr_query:type()) -> binary().
handle (Query) ->
   Input = parse_input(Query),
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
      handle(shr_query:new(A))
   }.
