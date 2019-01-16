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
      category :: category(),
      name :: binary(),
      deadline :: ataxia_time:type(),
      is_players_turn :: boolean(),
      is_pending :: boolean()
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
      get_deadline/1,
      get_mode/1,
      get_category/1,
      is_players_turn/1,
      is_pending/1
   ]
).
-export
(
   [
      set_id/2,
      set_name/2,
      set_deadline/2,
      set_mode/2,
      set_category/2,
      set_is_players_turn/2,
      set_is_pending/2
   ]
).

-export
(
   [
      get_id_field/0,
      get_name_field/0,
      get_deadline_field/0,
      get_mode_field/0,
      get_category_field/0,
      get_is_players_turn_field/0,
      get_is_pending_field/0
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
-spec new (ataxia_id:type(), binary(), mode(), category()) -> type().
new (ID, Name, Mode, Category) ->
   #battle_summary
   {
      id = ID,
      name = Name,
      mode = Mode,
      category = Category,
      deadline = ataxia_time:never(),
      is_players_turn = false,
      is_pending = false
   }.

-spec none () -> type().
none () ->
   #battle_summary
   {
      id = ataxia_id:null(),
      name = <<"">>,
      mode = none,
      category = event,
      deadline = ataxia_time:never(),
      is_players_turn = false,
      is_pending = false
   }.

%%%% Accessors
-spec get_id (type()) -> ataxia_id:type().
get_id (BattleSummary) -> BattleSummary#battle_summary.id.

-spec get_name (type()) -> binary().
get_name (BattleSummary) -> BattleSummary#battle_summary.name.

-spec get_deadline (type()) -> ataxia_time:type().
get_deadline (BattleSummary) -> BattleSummary#battle_summary.deadline.

-spec get_mode (type()) -> mode().
get_mode (BattleSummary) -> BattleSummary#battle_summary.mode.

-spec get_category (type()) -> category().
get_category (BattleSummary) -> BattleSummary#battle_summary.category.

-spec is_players_turn (type()) -> boolean().
is_players_turn (BattleSummary) -> BattleSummary#battle_summary.is_players_turn.

-spec is_pending (type()) -> boolean().
is_pending (BattleSummary) -> BattleSummary#battle_summary.is_pending.


-spec set_id (ataxia_id:type(), type()) -> type().
set_id (Val, BattleSummary) -> BattleSummary#battle_summary{ id = Val }.

-spec set_name (binary(), type()) -> type().
set_name (Val, BattleSummary) -> BattleSummary#battle_summary{ name = Val }.

-spec set_mode (mode(), type()) -> type().
set_mode (Val, BattleSummary) -> BattleSummary#battle_summary{ mode = Val }.

-spec set_category (category(), type()) -> type().
set_category (Val, BattleSummary) ->
   BattleSummary#battle_summary{ category = Val }.

-spec set_deadline (ataxia_time:type(), type()) -> type().
set_deadline (Val, BattleSummary) ->
   BattleSummary#battle_summary{ deadline = Val }.

-spec set_is_players_turn (boolean(), type()) -> type().
set_is_players_turn (Val, BattleSummary) ->
   BattleSummary#battle_summary{ is_players_turn = Val }.

-spec set_is_pending (boolean(), type()) -> type().
set_is_pending (Val, BattleSummary) ->
   BattleSummary#battle_summary{ is_pending = Val }.


-spec get_id_field () -> non_neg_integer().
get_id_field () -> #battle_summary.id.

-spec get_name_field () -> non_neg_integer().
get_name_field () -> #battle_summary.name.

-spec get_mode_field () -> non_neg_integer().
get_mode_field () -> #battle_summary.mode.

-spec get_category_field () -> non_neg_integer().
get_category_field () -> #battle_summary.category.

-spec get_deadline_field () -> non_neg_integer().
get_deadline_field () -> #battle_summary.deadline.

-spec get_is_players_turn_field () -> non_neg_integer().
get_is_players_turn_field () -> #battle_summary.is_players_turn.

-spec get_is_pending_field () -> non_neg_integer().
get_is_pending_field () -> #battle_summary.is_pending.


-spec encode ({non_neg_integer(), type()}) -> {list(any())}.
encode ({IX, BattleSummary}) ->
   {
      [
         {<<"ix">>, IX},
         {<<"id">>, BattleSummary#battle_summary.id},
         {<<"nme">>, BattleSummary#battle_summary.name},
         {
            <<"dln">>,
            ataxia_time:to_string(BattleSummary#battle_summary.deadline)
         },
         {<<"ipt">>, BattleSummary#battle_summary.is_players_turn}
      ]
   }.
