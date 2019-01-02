-module(btl_pending_battle).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-type id() :: binary().

-record
(
   pending_battle,
   {
      id :: id(),
      free_slots :: non_neg_integer(),
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
      get_id/1,
      get_battle/1,
      get_free_slots/1,

      set_battle/2,
      set_free_slots/2,

      get_battle_field/0,
      get_free_slots_field/0
   ]
).

-export
(
   [
      new/3
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec new (id(), non_neg_integer(), btl_battle:type()) -> type().
new (ID, FreeSlots, Battle) ->
   #pending_battle
   {
      id = ID,
      free_slots = FreeSlots,
      battle = Battle
   }.

%%%% Accessors
-spec get_id (type()) -> id().
get_id (PBattle) -> PBattle#pending_battle.id.

-spec get_battle (type()) -> btl_battle:type().
get_battle (PBattle) -> PBattle#pending_battle.battle.

-spec get_free_slots (type()) -> non_neg_integer().
get_free_slots (PBattle) -> PBattle#pending_battle.free_slots.

-spec set_battle (btl_battle:type(), type()) -> type().
set_battle (Battle, PBattle) -> PBattle#pending_battle{ battle = Battle }.

-spec set_free_slots (non_neg_integer(), type()) -> type().
set_free_slots (Val, PBattle) -> PBattle#pending_battle{ free_slots = Val }.

-spec get_battle_field () -> non_neg_integer().
get_battle_field () -> #pending_battle.battle.

-spec get_free_slots_field () -> non_neg_integer().
get_free_slots_field () -> #pending_battle.free_slots.
