-module(shr_equipment).

-define(PRIMARY_WEAPON_FIELD, <<"pr">>).
-define(SECONDARY_WEAPON_FIELD, <<"sc">>).
-define(ARMOR_FIELD, <<"ar">>).
-define(PORTRAIT_FIELD, <<"pt">>).
-define(GLYPH_BOARD_FIELD, <<"gb">>).
-define(GLYPHS_FIELD, <<"gl">>).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record
(
   shr_eq_ref,
   {
      primary :: shr_weapon:id(),
      secondary :: shr_weapon:id(),
      armor :: shr_armor:id(),
      portrait :: shr_portrait:id(),
      glyph_board :: shr_glyph_board:id(),
      glyphs :: list(shr_glyph:id())
   }
).

-record
(
   shr_eq,
   {
      primary :: shr_weapon:type(),
      secondary :: shr_weapon:type(),
      armor :: shr_armor:type(),
      portrait :: shr_portrait:type(),
      glyph_board :: shr_glyph_board:type(),
      glyphs :: list(shr_glyph:type())
   }
).

-opaque type() :: #shr_eq{}.
-opaque unresolved() :: #shr_eq_ref{}.
-type either() :: (type() | unresolved()).

-export_type([type/0, unresolved/0, either/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Accessors
-export
(
   [
      get_primary_weapon/1,
      get_secondary_weapon/1,
      get_armor/1,
      get_portrait/1,
      get_glyph_board/1,
      get_glyphs/1,

      set_primary_weapon/2,
      set_secondary_weapon/2,
      set_armor/2,
      set_portrait/2,
      set_glyph_board/2,
      set_glyphs/2,

      ataxia_set_primary_weapon/2,
      ataxia_set_secondary_weapon/2,
      ataxia_set_armor/2,
      ataxia_set_portrait/2,
      ataxia_set_glyph_board/2,
      ataxia_set_glyphs/2,
      ataxia_set_glyphs/3,

      get_primary_weapon_id/1,
      get_secondary_weapon_id/1,
      get_armor_id/1,
      get_portrait_id/1,
      get_glyph_board_id/1,
      get_glyph_ids/1,

      set_primary_weapon_id/2,
      set_secondary_weapon_id/2,
      set_armor_id/2,
      set_portrait_id/2,
      set_glyph_board_id/2,
      set_glyph_ids/2,

      ataxia_set_primary_weapon_id/2,
      ataxia_set_secondary_weapon_id/2,
      ataxia_set_armor_id/2,
      ataxia_set_portrait_id/2,
      ataxia_set_glyph_board_id/2,
      ataxia_set_glyph_ids/2,
      ataxia_set_glyph_ids/3
   ]
).

%%%%
-export
(
   [
      default/0,
      default_unresolved/0
   ]
).

-export
(
   [
      resolve/1,
      to_unresolved/1,
      encode/1,
      decode/1
   ]
).

-export
(
   [
      get_primary_weapon_field/0,
      get_secondary_weapon_field/0,
      get_armor_field/0,
      get_portrait_field/0,
      get_glyph_board_field/0,
      get_glyphs_field/0
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec get_primary_weapon (either()) -> shr_weapon:type().
get_primary_weapon (#shr_eq{ primary = R }) -> R;
get_primary_weapon (#shr_eq_ref{ primary = R }) -> shr_weapon:from_id(R).

-spec get_secondary_weapon (either()) -> shr_weapon:type().
get_secondary_weapon (#shr_eq{ secondary = R }) -> R;
get_secondary_weapon (#shr_eq_ref{ secondary = R }) -> shr_weapon:from_id(R).

-spec get_armor (either()) -> shr_armor:type().
get_armor (#shr_eq{ armor = R }) -> R;
get_armor (#shr_eq_ref{ armor = R }) -> shr_armor:from_id(R).

-spec get_portrait (either()) -> shr_portrait:type().
get_portrait (#shr_eq{ portrait = R }) -> R;
get_portrait (#shr_eq_ref{ portrait = R }) -> shr_portrait:from_id(R).

-spec get_glyph_board (either()) -> shr_glyph_board:type().
get_glyph_board (#shr_eq{ glyph_board = R }) -> R;
get_glyph_board (#shr_eq_ref{ glyph_board = R }) -> shr_glyph_board:from_id(R).

-spec get_glyphs (either()) -> list(shr_glyph:type()).
get_glyphs (#shr_eq{ glyphs = R }) -> R;
get_glyphs (#shr_eq_ref{ glyphs = R }) -> lists:map(fun shr_glyph:from_id/1, R).

-spec get_primary_weapon_id (either()) -> shr_weapon:id().
get_primary_weapon_id (#shr_eq_ref{ primary = R }) -> R;
get_primary_weapon_id (#shr_eq{ primary = R }) -> shr_weapon:get_id(R).

-spec get_secondary_weapon_id (either()) -> shr_weapon:id().
get_secondary_weapon_id (#shr_eq_ref{ secondary = R }) -> R;
get_secondary_weapon_id (#shr_eq{ secondary = R }) -> shr_weapon:get_id(R).

-spec get_armor_id (either()) -> shr_armor:id().
get_armor_id (#shr_eq_ref{ armor = R }) -> R;
get_armor_id (#shr_eq{ armor = R }) -> shr_armor:get_id(R).

-spec get_portrait_id (either()) -> shr_portrait:id().
get_portrait_id (#shr_eq_ref{ portrait = R }) -> R;
get_portrait_id (#shr_eq{ portrait = R }) -> shr_portrait:get_id(R).

-spec get_glyph_board_id (either()) -> shr_glyph_board:id().
get_glyph_board_id (#shr_eq_ref{ glyph_board = R }) -> R;
get_glyph_board_id (#shr_eq{ glyph_board = R }) -> shr_glyph_board:get_id(R).

-spec get_glyph_ids (either()) -> list(shr_glyph:id()).
get_glyph_ids (#shr_eq_ref{ glyphs = R }) -> R;
get_glyph_ids (#shr_eq{ glyphs = R }) -> lists:map(fun shr_glyph:get_id/1, R).

-spec set_primary_weapon
   (shr_weapon:type(), type()) -> type();
   (shr_weapon:type(), unresolved()) -> unresolved().
set_primary_weapon (V, Eq) when is_record(Eq, shr_eq) ->
   Eq#shr_eq{ primary = V };
set_primary_weapon (V, Eq) when is_record(Eq, shr_eq_ref) ->
   Eq#shr_eq_ref{ primary = shr_weapon:get_id(V) }.

-spec ataxia_set_primary_weapon
   (shr_weapon:type(), type()) -> {type(), ataxic:basic()};
   (shr_weapon:type(), unresolved()) -> {unresolved(), ataxic:basic()}.
ataxia_set_primary_weapon (V, Eq) ->
   {
      set_primary_weapon(V, Eq),
      ataxic:update_field
      (
         get_primary_weapon_field(),
         ataxic:constant(shr_weapon:get_id(V))
      )
   }.

-spec set_secondary_weapon
   (shr_weapon:type(), type()) -> type();
   (shr_weapon:type(), unresolved()) -> unresolved().
set_secondary_weapon (V, Eq) when is_record(Eq, shr_eq) ->
   Eq#shr_eq{ secondary = V };
set_secondary_weapon (V, Eq) when is_record(Eq, shr_eq_ref) ->
   Eq#shr_eq_ref{ secondary = shr_weapon:get_id(V) }.

-spec ataxia_set_secondary_weapon
   (shr_weapon:type(), type()) -> {type(), ataxic:basic()};
   (shr_weapon:type(), unresolved()) -> {unresolved(), ataxic:basic()}.
ataxia_set_secondary_weapon (V, Eq) ->
   {
      set_secondary_weapon(V, Eq),
      ataxic:update_field
      (
         get_secondary_weapon_field(),
         ataxic:constant(shr_weapon:get_id(V))
      )
   }.

-spec set_armor
   (shr_armor:type(), type()) -> type();
   (shr_armor:type(), unresolved()) -> unresolved().
set_armor (V, Eq) when is_record(Eq, shr_eq) ->
   Eq#shr_eq{ armor = V };
set_armor (V, Eq) when is_record(Eq, shr_eq_ref) ->
   Eq#shr_eq_ref{ armor = shr_armor:get_id(V) }.

-spec ataxia_set_armor
   (shr_armor:type(), type()) -> {type(), ataxic:basic()};
   (shr_armor:type(), unresolved()) -> {unresolved(), ataxic:basic()}.
ataxia_set_armor (V, Eq) ->
   {
      set_armor(V, Eq),
      ataxic:update_field
      (
         get_armor_field(),
         ataxic:constant(shr_armor:get_id(V))
      )
   }.

-spec set_portrait
   (shr_portrait:type(), type()) -> type();
   (shr_portrait:type(), unresolved()) -> unresolved().
set_portrait (V, Eq) when is_record(Eq, shr_eq) ->
   Eq#shr_eq{ portrait = V };
set_portrait (V, Eq) when is_record(Eq, shr_eq_ref) ->
   Eq#shr_eq_ref{ portrait = shr_portrait:get_id(V) }.

-spec ataxia_set_portrait
   (shr_portrait:type(), type()) -> {type(), ataxic:basic()};
   (shr_portrait:type(), unresolved()) -> {unresolved(), ataxic:basic()}.
ataxia_set_portrait (V, Eq) ->
   {
      set_portrait(V, Eq),
      ataxic:update_field
      (
         get_portrait_field(),
         ataxic:constant(shr_portrait:get_id(V))
      )
   }.

-spec set_glyph_board
   (shr_glyph_board:type(), type()) -> type();
   (shr_glyph_board:type(), unresolved()) -> unresolved().
set_glyph_board (V, Eq) when is_record(Eq, shr_eq) ->
   Eq#shr_eq{ glyph_board = V };
set_glyph_board (V, Eq) when is_record(Eq, shr_eq_ref) ->
   Eq#shr_eq_ref{ glyph_board = shr_glyph_board:get_id(V) }.

-spec ataxia_set_glyph_board
   (shr_glyph_board:type(), type()) -> {type(), ataxic:basic()};
   (shr_glyph_board:type(), unresolved()) -> {unresolved(), ataxic:basic()}.
ataxia_set_glyph_board (V, Eq) ->
   {
      set_glyph_board(V, Eq),
      ataxic:update_field
      (
         get_glyph_board_field(),
         ataxic:constant(shr_glyph_board:get_id(V))
      )
   }.

-spec set_glyphs
   (list(shr_glyph:type()), type()) -> type();
   (list(shr_glyph:type()), unresolved()) -> unresolved().
set_glyphs (V, Eq) when is_record(Eq, shr_eq) ->
   Eq#shr_eq{ glyphs = V };
set_glyphs (V, Eq) when is_record(Eq, shr_eq_ref) ->
   Eq#shr_eq_ref{ glyphs = lists:map(fun shr_glyph:get_id/1, V) }.

-spec ataxia_set_glyphs
   (list(shr_glyph:type()), type()) -> {type(), ataxic:basic()};
   (list(shr_glyph:type()), unresolved()) -> {unresolved(), ataxic:basic()}.
ataxia_set_glyphs (V, Eq) ->
   ataxia_set_glyphs
   (
      V,
      ataxic:constant(lists:map(fun shr_glyph:get_id/1, V)),
      Eq
   ).

-spec ataxia_set_glyphs
   (
      list(shr_glyph:type()),
      ataxic:basic(),
      type()
   )
   -> {type(), ataxic:basic()};
   (
      list(shr_glyph:type()),
      ataxic:basic(),
      unresolved()
   )
   -> {unresolved(), ataxic:basic()}.
ataxia_set_glyphs (V, VUpdate, Eq) ->
   {
      set_glyphs(V, Eq),
      ataxic:update_field
      (
         get_glyphs_field(),
         VUpdate
      )
   }.

-spec set_primary_weapon_id
   (shr_weapon:id(), type()) -> type();
   (shr_weapon:id(), unresolved()) -> unresolved().
set_primary_weapon_id (V, Eq) when is_record(Eq, shr_eq_ref) ->
   Eq#shr_eq_ref{ primary = V };
set_primary_weapon_id (V, Eq) when is_record(Eq, shr_eq) ->
   Eq#shr_eq{ primary = shr_weapon:from_id(V) }.

-spec ataxia_set_primary_weapon_id
   (shr_weapon:id(), type()) -> {type(), ataxic:basic()};
   (shr_weapon:id(), unresolved()) -> {unresolved(), ataxic:basic()}.
ataxia_set_primary_weapon_id (V, Eq) ->
   {
      set_primary_weapon_id(V, Eq),
      ataxic:update_field
      (
         get_primary_weapon_field(),
         ataxic:constant(V)
      )
   }.

-spec set_secondary_weapon_id
   (shr_weapon:id(), type()) -> type();
   (shr_weapon:id(), unresolved()) -> unresolved().
set_secondary_weapon_id (V, Eq) when is_record(Eq, shr_eq_ref) ->
   Eq#shr_eq_ref{ secondary = V };
set_secondary_weapon_id (V, Eq) when is_record(Eq, shr_eq) ->
   Eq#shr_eq{ secondary = shr_weapon:from_id(V) }.

-spec ataxia_set_secondary_weapon_id
   (shr_weapon:id(), type()) -> {type(), ataxic:basic()};
   (shr_weapon:id(), unresolved()) -> {unresolved(), ataxic:basic()}.
ataxia_set_secondary_weapon_id (V, Eq) ->
   {
      set_secondary_weapon_id(V, Eq),
      ataxic:update_field
      (
         get_secondary_weapon_field(),
         ataxic:constant(V)
      )
   }.

-spec set_armor_id
   (shr_armor:id(), type()) -> type();
   (shr_armor:id(), unresolved()) -> unresolved().
set_armor_id (V, Eq) when is_record(Eq, shr_eq_ref) ->
   Eq#shr_eq_ref{ armor = V };
set_armor_id (V, Eq) when is_record(Eq, shr_eq) ->
   Eq#shr_eq{ armor = shr_armor:from_id(V) }.

-spec ataxia_set_armor_id
   (shr_armor:id(), type()) -> {type(), ataxic:basic()};
   (shr_armor:id(), unresolved()) -> {unresolved(), ataxic:basic()}.
ataxia_set_armor_id (V, Eq) ->
   {
      set_armor_id(V, Eq),
      ataxic:update_field
      (
         get_armor_field(),
         ataxic:constant(V)
      )
   }.

-spec set_portrait_id
   (shr_portrait:id(), type()) -> type();
   (shr_portrait:id(), unresolved()) -> unresolved().
set_portrait_id (V, Eq) when is_record(Eq, shr_eq_ref) ->
   Eq#shr_eq_ref{ portrait = V };
set_portrait_id (V, Eq) when is_record(Eq, shr_eq) ->
   Eq#shr_eq{ portrait = shr_portrait:from_id(V) }.

-spec ataxia_set_portrait_id
   (shr_portrait:id(), type()) -> {type(), ataxic:basic()};
   (shr_portrait:id(), unresolved()) -> {unresolved(), ataxic:basic()}.
ataxia_set_portrait_id (V, Eq) ->
   {
      set_portrait_id(V, Eq),
      ataxic:update_field
      (
         get_portrait_field(),
         ataxic:constant(V)
      )
   }.

-spec set_glyph_board_id
   (shr_glyph_board:id(), type()) -> type();
   (shr_glyph_board:id(), unresolved()) -> unresolved().
set_glyph_board_id (V, Eq) when is_record(Eq, shr_eq_ref) ->
   Eq#shr_eq_ref{ glyph_board = V };
set_glyph_board_id (V, Eq) when is_record(Eq, shr_eq) ->
   Eq#shr_eq{ glyph_board = shr_glyph_board:from_id(V) }.

-spec ataxia_set_glyph_board_id
   (shr_glyph_board:id(), type()) -> {type(), ataxic:basic()};
   (shr_glyph_board:id(), unresolved()) -> {unresolved(), ataxic:basic()}.
ataxia_set_glyph_board_id (V, Eq) ->
   {
      set_glyph_board_id(V, Eq),
      ataxic:update_field
      (
         get_glyph_board_field(),
         ataxic:constant(V)
      )
   }.

-spec set_glyph_ids
   (list(shr_glyph:id()), type()) -> type();
   (list(shr_glyph:id()), unresolved()) -> unresolved().
set_glyph_ids (V, Eq) when is_record(Eq, shr_eq_ref) ->
   Eq#shr_eq_ref{ glyphs = V };
set_glyph_ids (V, Eq) when is_record(Eq, shr_eq) ->
   Eq#shr_eq{ glyphs = lists:map(fun shr_glyph:from_id/1, V) }.

-spec ataxia_set_glyph_ids
   (list(shr_glyph:id()), type()) -> {type(), ataxic:basic()};
   (list(shr_glyph:id()), unresolved()) -> {unresolved(), ataxic:basic()}.
ataxia_set_glyph_ids (V, Eq) ->
   ataxia_set_glyph_ids(V, ataxic:constant(V), Eq).

-spec ataxia_set_glyph_ids
   (
      list(shr_glyph:id()),
      ataxic:basic(),
      type()
   )
   -> {type(), ataxic:basic()};
   (
      list(shr_glyph:id()),
      ataxic:basic(),
      unresolved()
   )
   -> {unresolved(), ataxic:basic()}.
ataxia_set_glyph_ids (V, VUpdate, Eq) ->
   {
      set_glyph_ids(V, Eq),
      ataxic:update_field(get_glyphs_field(), VUpdate)
   }.

-spec default () -> type().
default () ->
   #shr_eq
   {
      primary = shr_weapon:default(),
      secondary = shr_weapon:default(),
      armor = shr_armor:default(),
      portrait = shr_portrait:default(),
      glyph_board = shr_glyph_board:default(),
      glyphs = []
   }.

-spec default_unresolved () -> unresolved().
default_unresolved () ->
   #shr_eq_ref
   {
      primary = shr_weapon:default_id(),
      secondary = shr_weapon:default_id(),
      armor = shr_armor:default_id(),
      portrait = shr_portrait:default_id(),
      glyph_board = shr_glyph_board:default_id(),
      glyphs = []
   }.

-spec decode (map()) -> unresolved().
decode (Map) ->
   #shr_eq_ref
   {
      primary = maps:get(?PRIMARY_WEAPON_FIELD, Map),
      secondary = maps:get(?SECONDARY_WEAPON_FIELD, Map),
      armor = maps:get(?ARMOR_FIELD, Map),
      portrait = maps:get(?PORTRAIT_FIELD, Map),
      glyph_board = maps:get(?GLYPH_BOARD_FIELD, Map),
      glyphs = maps:get(?GLYPHS_FIELD, Map)
   }.

-spec encode (unresolved()) -> {list({binary(), any()})}.
encode (EqRef) ->
   {
      [
         {?PRIMARY_WEAPON_FIELD, EqRef#shr_eq_ref.primary},
         {?SECONDARY_WEAPON_FIELD, EqRef#shr_eq_ref.secondary},
         {?ARMOR_FIELD, EqRef#shr_eq_ref.armor},
         {?PORTRAIT_FIELD, EqRef#shr_eq_ref.portrait},
         {?GLYPH_BOARD_FIELD, EqRef#shr_eq_ref.glyph_board},
         {?GLYPHS_FIELD, EqRef#shr_eq_ref.glyphs}
      ]
   }.

-spec resolve (unresolved()) -> type().
resolve (EqRef) ->
   #shr_eq
   {
      primary = shr_weapon:from_id(EqRef#shr_eq_ref.primary),
      secondary = shr_weapon:from_id(EqRef#shr_eq_ref.secondary),
      armor = shr_armor:from_id(EqRef#shr_eq_ref.armor),
      portrait = shr_portrait:from_id(EqRef#shr_eq_ref.portrait),
      glyph_board = shr_glyph_board:from_id(EqRef#shr_eq_ref.glyph_board),
      glyphs = lists:map(fun shr_glyph:from_id/1, EqRef#shr_eq_ref.glyphs)
   }.

-spec to_unresolved (type()) -> unresolved().
to_unresolved (Eq) ->
   #shr_eq_ref
   {
      primary = shr_weapon:get_id(Eq#shr_eq.primary),
      secondary = shr_weapon:get_id(Eq#shr_eq.secondary),
      armor = shr_armor:get_id(Eq#shr_eq.armor),
      portrait = shr_portrait:get_id(Eq#shr_eq.portrait),
      glyph_board = shr_glyph_board:get_id(Eq#shr_eq.glyph_board),
      glyphs = lists:map(fun shr_glyph:get_id/1, Eq#shr_eq.glyphs)
   }.

-spec get_primary_weapon_field () -> non_neg_integer().
get_primary_weapon_field () -> #shr_eq_ref.primary.

-spec get_secondary_weapon_field () -> non_neg_integer().
get_secondary_weapon_field () -> #shr_eq_ref.secondary.

-spec get_armor_field () -> non_neg_integer().
get_armor_field () -> #shr_eq_ref.armor.

-spec get_portrait_field () -> non_neg_integer().
get_portrait_field () -> #shr_eq_ref.portrait.

-spec get_glyph_board_field () -> non_neg_integer().
get_glyph_board_field () -> #shr_eq_ref.glyph_board.

-spec get_glyphs_field () -> non_neg_integer().
get_glyphs_field () -> #shr_eq_ref.glyphs.
