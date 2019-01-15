-module(btl_pending_battle).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-type id() :: ataxia_id:type().

-record
(
   pending_battle,
   {
      free_slots :: non_neg_integer(),
      player_ids :: list(shr_player:id()),
      player_summary_ixs :: list(non_neg_integer()),
      battle_id :: btl_battle:id(),
      battle :: btl_battle:type()
   }
).

-opaque type() :: #pending_battle{}.

-export_type([type/0, id/0]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Accessors
-export
(
   [
      get_battle/1,
      get_free_slots/1,
      get_battle_id/1,
      get_player_ids/1,
      get_player_summary_ixs/1,

      set_battle/2,
      set_free_slots/2,
      set_battle_id/2,
      set_player_ids/2,
      set_player_summary_ixs/2,

      get_battle_field/0,
      get_free_slots_field/0,
      get_battle_id_field/0,
      get_player_ids_field/0,
      get_player_summary_ixs_field/0
   ]
).

-export
(
   [
      new/2
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec new (non_neg_integer(), btl_battle:type()) -> type().
new (FreeSlots, Battle) ->
   #pending_battle
   {
      free_slots = FreeSlots,
      player_ids = [],
      player_summary_ixs = [],
      battle_id = ataxia_id:null(),
      battle = Battle
   }.

%%%% Accessors
-spec get_battle_id (type()) -> btl_battle:id().
get_battle_id (PBattle) -> PBattle#pending_battle.battle_id.

-spec get_battle (type()) -> btl_battle:type().
get_battle (PBattle) -> PBattle#pending_battle.battle.

-spec get_free_slots (type()) -> non_neg_integer().
get_free_slots (PBattle) -> PBattle#pending_battle.free_slots.

-spec get_player_ids (type()) -> list(shr_player:id()).
get_player_ids (PBattle) -> PBattle#pending_battle.player_ids.

-spec get_player_summary_ixs (type()) -> list(non_neg_integer()).
get_player_summary_ixs (PBattle) -> PBattle#pending_battle.player_summary_ixs.

-spec set_battle (btl_battle:type(), type()) -> type().
set_battle (Battle, PBattle) -> PBattle#pending_battle{ battle = Battle }.

-spec set_battle_id (btl_battle:id(), type()) -> type().
set_battle_id (BattleID, PBattle) ->
   PBattle#pending_battle{ battle_id = BattleID }.

-spec set_free_slots (non_neg_integer(), type()) -> type().
set_free_slots (Val, PBattle) -> PBattle#pending_battle{ free_slots = Val }.

-spec set_player_summary_ixs (list(non_neg_integer()), type()) -> type().
set_player_summary_ixs (Val, PBattle) ->
   PBattle#pending_battle{ player_summary_ixs = Val }.

-spec set_player_ids (list(shr_player:id()), type()) -> type().
set_player_ids (Val, PBattle) -> PBattle#pending_battle{ player_ids = Val }.

-spec get_battle_id_field () -> non_neg_integer().
get_battle_id_field () -> #pending_battle.battle_id.

-spec get_battle_field () -> non_neg_integer().
get_battle_field () -> #pending_battle.battle.

-spec get_free_slots_field () -> non_neg_integer().
get_free_slots_field () -> #pending_battle.free_slots.

-spec get_player_ids_field () -> non_neg_integer().
get_player_ids_field () -> #pending_battle.player_ids.

-spec get_player_summary_ixs_field () -> non_neg_integer().
get_player_summary_ixs_field () -> #pending_battle.player_summary_ixs.
