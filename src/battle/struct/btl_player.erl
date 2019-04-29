-module(btl_player).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record
(
   player,
   {
      id :: shr_player:id(),
      character_ix :: non_neg_integer(),
      timeline :: list(any()),
      is_active :: boolean(),
      luck :: integer(),
      summary_ix :: non_neg_integer(),
      summary_category :: shr_battle_summary:category()
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
      get_luck/1,
      get_summary_index/1,
      get_summary_category/1,
      get_character_index/1,
      get_timeline/1,

      get_is_active/1,

      set_is_active/2,
      ataxia_set_is_active/2,

      set_luck/2,
      ataxia_set_luck/2,

      add_to_timeline/2,
      ataxia_add_to_timeline/2,

      reset_timeline/1,
      ataxia_reset_timeline/1,

      get_timeline_field/0,
      get_luck_field/0,
      get_is_active_field/0
   ]
).

-export
(
   [
      new/4
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

-spec get_luck (type()) -> integer().
get_luck (Player) -> Player#player.luck.

-spec get_summary_index (type()) -> non_neg_integer().
get_summary_index (Player) -> Player#player.summary_ix.

-spec get_summary_category (type()) -> shr_battle_summary:category().
get_summary_category (Player) -> Player#player.summary_category.

-spec get_character_index (type()) -> non_neg_integer().
get_character_index (Player) -> Player#player.character_ix.

-spec get_timeline (type()) -> list(any()).
get_timeline (Player) -> Player#player.timeline.

-spec get_is_active (type()) -> boolean().
get_is_active (Player) -> Player#player.is_active.


-spec set_is_active (boolean(), type()) -> type().
set_is_active (Val, Player) -> Player#player{ is_active = Val }.

-spec ataxia_set_is_active (boolean(), type()) -> {type(), ataxic:basic()}.
ataxia_set_is_active (Val, Player) ->
   {
      Player#player{ is_active = Val },
      ataxic:update_field
      (
         get_is_active_field(),
         ataxic:constant(Val)
      )
   }.

-spec set_luck (integer(), type()) -> type().
set_luck (Val, Player) -> Player#player{ luck = Val }.

-spec ataxia_set_luck (integer(), type()) -> {type(), ataxic:basic()}.
ataxia_set_luck (Val, Player) ->
   {
      Player#player{ luck = Val },
      ataxic:update_field
      (
         get_luck_field(),
         ataxic:constant(Val)
      )
   }.

-spec add_to_timeline (list(any()), type()) -> type().
add_to_timeline (NewEvents, Player) ->
   Player#player
   {
      timeline = (NewEvents ++ Player#player.timeline)
   }.

-spec ataxia_add_to_timeline (list(any()), type()) -> {type(), ataxic:basic()}.
ataxia_add_to_timeline (NewEvents, Player) ->
   {
      Player#player{ timeline = (NewEvents ++ Player#player.timeline) },
      ataxic:update_field
      (
         get_timeline_field(),
         ataxic:apply_function
         (
            lists,
            append,
            [
               ataxic:constant(NewEvents),
               ataxic:current_value()
            ]
         )
      )
   }.

-spec reset_timeline (type()) -> type().
reset_timeline (Player) -> Player#player{ timeline = [] }.

-spec ataxia_reset_timeline (type()) -> {type(), ataxic:basic()}.
ataxia_reset_timeline (Player) ->
   {
      Player#player{ timeline = [] },
      ataxic:update_field
      (
         get_timeline_field(),
         ataxic:constant([])
      )
   }.

-spec new
   (
      non_neg_integer(),
      shr_player:id(),
      non_neg_integer(),
      shr_battle_summary:category()
   ) -> type().
new (CharacterIX, ID, SummaryIX, SummaryCategory) ->
   #player
   {
      character_ix = CharacterIX,
      id = ID,
      is_active = true,
      timeline = [],
      luck = 0,
      summary_ix = SummaryIX,
      summary_category = SummaryCategory
   }.

-spec get_timeline_field () -> non_neg_integer().
get_timeline_field () -> #player.timeline.

-spec get_luck_field () -> non_neg_integer().
get_luck_field () -> #player.luck.

-spec get_is_active_field () -> non_neg_integer().
get_is_active_field () -> #player.is_active.
