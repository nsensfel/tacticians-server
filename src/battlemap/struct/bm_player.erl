-module(bm_player).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-type id() :: binary().

-record
(
   player,
   {
      ix :: non_neg_integer(),
      id :: id(),
      timeline :: list(any())
   }
).

-opaque type() :: #player{}.

-export_type([type/0, id/0]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export
(
   [
      get_id/1,
      get_index/1,
      get_timeline/1,
      add_to_timeline/2,
      reset_timeline/1,

      get_timeline_field/0
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
-spec get_id (type()) -> id().
get_id (Player) -> Player#player.id.

-spec get_index (type()) -> non_neg_integer().
get_index (Player) -> Player#player.ix.

-spec get_timeline (type()) -> list(any()).
get_timeline (Player) -> Player#player.timeline.

-spec add_to_timeline (list(any()), type()) -> type().
add_to_timeline (NewEvents, Player) ->
   OldTimeline = Player#player.timeline,

   Player#player
   {
      timeline = (NewEvents ++ OldTimeline)
   }.

-spec reset_timeline (type()) -> type().
reset_timeline (Player) -> Player#player{ timeline = [] }.

-spec new (non_neg_integer(), id()) -> type().
new (IX, ID) ->
   #player
   {
      ix = IX,
      id = ID,
      timeline = []
   }.

-spec get_timeline_field () -> non_neg_integer().
get_timeline_field () -> #player.timeline.
