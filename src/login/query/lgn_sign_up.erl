-module(lgn_sign_up).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record
(
   input,
   {
      username :: binary(),
      password :: binary(),
      email :: binary()
   }
).

-record
(
   query_state,
   {
      player :: shr_player:type(),
      player_id :: shr_player:id()
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
   Email = maps:get(<<"eml">>, JSONReqMap),

   #input
   {
      username = Username,
      password = Password,
      email = Email
   }.

-spec register_user (input()) -> query_state().
register_user (Input) ->
   Username = Input#input.username,
   Password = Input#input.password,
   Email = Input#input.email,

   {GeneratedPlayerID, GeneratedPlayer} =
      bnt_generate_player:attempt(Username, Password, Email),

   #query_state
   {
      player = GeneratedPlayer,
      player_id = GeneratedPlayerID
   }.

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
   %% TODO [SECURITY][LOW]: validate input size.
   QueryState = register_user(Input),
   generate_reply(QueryState).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
out(A) ->
   {
      content,
      "application/json; charset=UTF-8",
      handle(shr_query:new(A))
   }.
