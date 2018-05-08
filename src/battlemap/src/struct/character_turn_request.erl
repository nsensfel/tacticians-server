-module(character_turn_request).

-define(PLAYER_ID_FIELD, <<"pid">>).
-define(SESSION_TOKEN_FIELD, <<"stk">>).
-define(BATTLE_ID_FIELD, <<"bid">>).
-define(CHAR_INST_IX_FIELD, <<"cix">>).
-define(ACTIONS_FIELD, <<"act">>).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record
(
   type,
   {
      player_id :: player:id(),
      session_token :: binary(),
      battle_id :: binary(),
      character_instance_ix :: non_neg_integer(),
      actions :: list(battle_action:type())
   }
).

-opaque type() :: #type{}.

-export_type([type/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export
(
   [
      decode/1
   ]
).

-export
(
   [
      get_player_id/1,
      get_session_token/1,
      get_battle_id/1,
      get_character_instance_ix/1,
      get_actions/1
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec decode (map()) -> type().
decode (Map) ->
   CharacterInstanceIX = binary_to_integer(maps:get(?CHAR_INST_IX_FIELD, Map)),
   EncodedActions = maps:get(?ACTIONS_FIELD, Map),
   Actions = lists:map(fun battle_action:decode/1, EncodedActions),

   #type
   {
      player_id = maps:get(?PLAYER_ID_FIELD, Map),
      session_token = maps:get(?SESSION_TOKEN_FIELD, Map),
      battle_id = maps:get(?BATTLE_ID_FIELD, Map),
      character_instance_ix = CharacterInstanceIX,
      actions = Actions
   }.

-spec get_player_id (type()) -> player:id().
get_player_id (Request) -> Request#type.player_id.

-spec get_session_token (type()) -> binary().
get_session_token (Request) -> Request#type.session_token.

-spec get_battle_id (type()) -> binary().
get_battle_id (Request) -> Request#type.battle_id.

-spec get_character_instance_ix (type()) -> non_neg_integer().
get_character_instance_ix (Request) -> Request#type.character_instance_ix.

-spec get_actions (type()) -> list(battle_action:type()).
get_actions (Request) -> Request#type.actions.
