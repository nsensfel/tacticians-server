-module(shr_battle_summary).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-type mode() :: (attack | defend | none).
-type category() :: (event | invasion | campaign).

-record
(
   battle_summary,
   {
      id :: ataxia_id:type(),
      mode :: mode(),
      name :: binary(),
      last_edit :: binary(),
      is_players_turn :: boolean()
   }
).

-opaque type() :: #battle_summary{}.

-export_type([type/0, mode/0, category/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export
(
   [
      new/4,
      none/0
   ]
).

%%%% Accessors
-export
(
   [
      get_id/1,
      get_name/1,
      get_last_edit/1,
      is_players_turn/1
   ]
).
-export
(
   [
      set_id/2,
      set_name/2,
      set_last_edit/2,
      set_is_players_turn/2
   ]
).

-export
(
   [
      get_id_field/0,
      get_name_field/0,
      get_last_edit_field/0,
      get_is_players_turn_field/0
   ]
).

%%%% Export
-export
(
   [
      encode/1
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec new (binary(), binary(), binary(), boolean()) -> type().
new (ID, Name, Time, IsPlayersTurn) ->
   #battle_summary
   {
      id = ID,
      name = Name,
      mode = none,
      last_edit = Time,
      is_players_turn = IsPlayersTurn
   }.

-spec none () -> type().
none () ->
   #battle_summary
   {
      id = <<"">>,
      name = <<"">>,
      mode = none,
      last_edit = <<"">>,
      is_players_turn = false
   }.

%%%% Accessors
-spec get_id (type()) -> binary().
get_id (BattleSummary) -> BattleSummary#battle_summary.id.

-spec get_name (type()) -> binary().
get_name (BattleSummary) -> BattleSummary#battle_summary.name.

-spec get_last_edit (type()) -> binary().
get_last_edit (BattleSummary) -> BattleSummary#battle_summary.last_edit.

-spec is_players_turn (type()) -> boolean().
is_players_turn (BattleSummary) -> BattleSummary#battle_summary.is_players_turn.

-spec set_id (binary(), type()) -> type().
set_id (Val, BattleSummary) -> BattleSummary#battle_summary{ id = Val }.

-spec set_name (binary(), type()) -> type().
set_name (Val, BattleSummary) -> BattleSummary#battle_summary{ name = Val }.

-spec set_last_edit (binary(), type()) -> type().
set_last_edit (Val, BattleSummary) ->
   BattleSummary#battle_summary{ last_edit = Val }.

-spec set_is_players_turn (boolean(), type()) -> type().
set_is_players_turn (Val, BattleSummary) ->
   BattleSummary#battle_summary{ is_players_turn = Val }.

-spec get_id_field () -> non_neg_integer().
get_id_field () -> #battle_summary.id.

-spec get_name_field () -> non_neg_integer().
get_name_field () -> #battle_summary.name.

-spec get_last_edit_field () -> non_neg_integer().
get_last_edit_field () -> #battle_summary.last_edit.

-spec get_is_players_turn_field () -> non_neg_integer().
get_is_players_turn_field () -> #battle_summary.is_players_turn.

-spec encode ({non_neg_integer(), type()}) -> {list(any())}.
encode ({IX, BattleSummary}) ->
   {
      [
         {<<"ix">>, IX},
         {<<"id">>, BattleSummary#battle_summary.id},
         {<<"nme">>, BattleSummary#battle_summary.name},
         {<<"ldt">>, BattleSummary#battle_summary.last_edit},
         {<<"ipt">>, BattleSummary#battle_summary.is_players_turn}
      ]
   }.
