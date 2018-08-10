-module(lgn_sign_up).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-include("../../../include/yaws_api.hrl").

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

   shr_janitor:new(login_db, Username),
   shr_janitor:new(login_db, Email),

   ok = shr_database:reserve(login_db, Username, janitor),
   ok = shr_database:reserve(login_db, Email, janitor),

   Player = shr_player:new(<<"">>, Username, Password, Email),

   {ok, PlayerID} = shr_database:insert(player_db, janitor, janitor, Player),

   shr_janitor:new(player_db, PlayerID),

   LoginUpdateQueryOps =
      [
         shr_db_query:set_value(PlayerID),
         shr_db_query:set_read_permission(any),
         shr_db_query:set_write_permission([{user, PlayerID}])
      ],

   PlayerUpdateQueryOps =
      [
         shr_db_query:set_field(shr_player:get_id_field(), PlayerID),
         shr_db_query:set_read_permission(any),
         shr_db_query:set_write_permission([{user, PlayerID}])
      ],

   ok =
      shr_database:commit
      (
         shr_db_query:new(login_db, Username, janitor, LoginUpdateQueryOps)
      ),

   ok =
      shr_database:commit
      (
         shr_db_query:new(login_db, Email, janitor, LoginUpdateQueryOps)
      ),

   ok =
      shr_database:commit
      (
         shr_db_query:new(player_db, PlayerID, janitor, PlayerUpdateQueryOps)
      ),

   #query_state
   {
      player = shr_player:set_id(PlayerID, Player)
   }.

-spec generate_reply(query_state()) -> binary().
generate_reply (QueryState) ->
   Player = QueryState#query_state.player,

   SetSession = lgn_set_session:generate(Player),
   Output = jiffy:encode([SetSession]),

   Output.

-spec handle (binary()) -> binary().
handle (Req) ->
   Input = parse_input(Req),
   %% TODO: validate input
   QueryState = register_user(Input),
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
