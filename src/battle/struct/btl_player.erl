-module(btl_player).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record
(
   player,
   {
      ix :: non_neg_integer(),
      id :: shr_player:id(),
      character_ix :: non_neg_integer(),
      timeline :: list(any()),
      is_active :: boolean()
   }
).

-opaque type() :: #player{}.

-export_type([type/0]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export
(
   [
      get_id/1,
      get_index/1,
      get_character_index/1,
      get_timeline/1,

      get_is_active/1,
      set_is_active/2,

      add_to_timeline/2,
      reset_timeline/1,

      get_timeline_field/0,
      get_is_active_field/0
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
-spec get_id (type()) -> shr_player:id().
get_id (Player) -> Player#player.id.

-spec get_index (type()) -> non_neg_integer().
get_index (Player) -> Player#player.ix.

-spec get_character_index (type()) -> non_neg_integer().
get_character_index (Player) -> Player#player.character_ix.

-spec get_timeline (type()) -> list(any()).
get_timeline (Player) -> Player#player.timeline.

-spec get_is_active (type()) -> boolean().
get_is_active (Player) -> Player#player.is_active.

-spec set_is_active (boolean(), type()) -> type().
set_is_active (Val, Player) -> Player#player{ is_active = Val }.

-spec add_to_timeline (list(any()), type()) -> type().
add_to_timeline (NewEvents, Player) ->
   OldTimeline = Player#player.timeline,

   Player#player
   {
      timeline = (NewEvents ++ OldTimeline)
   }.

-spec reset_timeline (type()) -> type().
reset_timeline (Player) -> Player#player{ timeline = [] }.

-spec new (non_neg_integer(), non_neg_integer(), shr_player:id()) -> type().
new (IX, CharacterIX, ID) ->
   #player
   {
      ix = IX,
      character_ix = CharacterIX,
      id = ID,
      is_active = true,
      timeline = []
   }.

-spec get_timeline_field () -> non_neg_integer().
get_timeline_field () -> #player.timeline.

-spec get_is_active_field () -> non_neg_integer().
get_is_active_field () -> #player.is_active.
