-module(btl_character_turn_request).

-define(PLAYER_ID_FIELD, <<"pid">>).
-define(SESSION_TOKEN_FIELD, <<"stk">>).
-define(BATTLE_ID_FIELD, <<"bid">>).
-define(CHAR_IX_FIELD, <<"cix">>).
-define(ACTIONS_FIELD, <<"act">>).
-define(ACTIONS_MOVE_FIELD, <<"mov">>).
-define(ACTIONS_WPS_FIELD, <<"wps">>).
-define(ACTIONS_ATK_FIELD, <<"tar">>).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record
(
   type,
   {
      player_id :: shr_player:id(),
      session_token :: binary(),
      battle_id :: binary(),
      character_ix :: non_neg_integer(),
      actions :: list(btl_battle_action:type())
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
      get_character_ix/1,
      get_actions/1
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec decode_actions (map()) -> list(btl_battle_action:type()).
decode_actions (Act) ->
   S0Result = [],
   S1Result =
      case
         btl_battle_action:maybe_decode_move(maps:get(?ACTIONS_MOVE_FIELD, Act))
      of
         [] -> S0Result;
         [Move] -> [Move|S0Result]
      end,

   S2Result =
      case
         btl_battle_action:maybe_decode_atk(maps:get(?ACTIONS_ATK_FIELD, Act))
      of
         [] -> S1Result;
         [Atk] -> [Atk|S1Result]
      end,

   S3Result =
      case
         btl_battle_action:maybe_decode_move(maps:get(?ACTIONS_WPS_FIELD, Act))
      of
         [] -> S2Result;
         [Wps] -> [Wps|S2Result]
      end,

   S3Result.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec decode (map()) -> type().
decode (Map) ->
   CharacterIX = maps:get(?CHAR_IX_FIELD, Map),
   EncodedActions = maps:get(?ACTIONS_FIELD, Map),
   Actions = decode_actions(EncodedActions),

   #type
   {
      player_id = maps:get(?PLAYER_ID_FIELD, Map),
      session_token = maps:get(?SESSION_TOKEN_FIELD, Map),
      battle_id = maps:get(?BATTLE_ID_FIELD, Map),
      character_ix = CharacterIX,
      actions = Actions
   }.

-spec get_player_id (type()) -> shr_player:id().
get_player_id (Request) -> Request#type.player_id.

-spec get_session_token (type()) -> binary().
get_session_token (Request) -> Request#type.session_token.

-spec get_battle_id (type()) -> binary().
get_battle_id (Request) -> Request#type.battle_id.

-spec get_character_ix (type()) -> non_neg_integer().
get_character_ix (Request) -> Request#type.character_ix.

-spec get_actions (type()) -> list(btl_battle_action:type()).
get_actions (Request) -> Request#type.actions.
