-module(shr_inventory).

-define(WEAPONS_FIELD, <<"wp">>).
-define(ARMORS_FIELD, <<"ar">>).
-define(PORTRAITS_FIELD, <<"pt">>).
-define(GLYPH_BOARDS_FIELD, <<"gb">>).
-define(GLYPHS_FIELD, <<"gl">>).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record
(
   inventory,
   {
      weapons :: ordsets:ordset(shr_weapon:id()),
      armors :: ordsets:ordset(shr_armor:id()),
      portraits :: ordsets:ordset(shr_portrait:id()),
      glyph_boards :: ordsets:ordset(shr_glyph_board:id()),
      glyphs :: ordsets:ordset(shr_glyph:id())
   }
).

-opaque type() :: #inventory{}.
-type id() :: ataxia_id:type().

-export_type([type/0, id/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Accessors
-export
(
   [
      get_weapons/1,
      get_armors/1,
      get_portraits/1,
      get_glyph_boards/1,
      get_glyphs/1,

      add_weapon/2,
      add_armor/2,
      add_portrait/2,
      add_glyph_board/2,
      add_glyph/2,

      ataxia_add_armor/2,
      ataxia_add_weapon/2,
      ataxia_add_portrait/2,
      ataxia_add_glyph_board/2,
      ataxia_add_glyph/2
   ]
).

%%%% Accessors
-export
(
   [
      encode/1,
      decode/1
   ]
).

-export
(
   [
      default/0,
      allows_equipment/2,
      add_equipment/2,
      ataxia_add_equipment/2
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec get_weapons (type()) -> ordsets:ordset(shr_weapon:id()).
get_weapons (Inv) -> Inv#inventory.weapons.

-spec get_armors (type()) -> ordsets:ordset(shr_armor:id()).
get_armors (Inv) -> Inv#inventory.armors.

-spec get_portraits (type()) -> ordsets:ordset(shr_portrait:id()).
get_portraits (Inv) -> Inv#inventory.portraits.

-spec get_glyph_boards (type()) -> ordsets:ordset(shr_glyph_board:id()).
get_glyph_boards (Inv) -> Inv#inventory.glyph_boards.

-spec get_glyphs (type()) -> ordsets:ordset(shr_glyph:id()).
get_glyphs (Inv) -> Inv#inventory.glyphs.


-spec add_weapon (shr_weapon:id(), type()) -> type().
add_weapon (V, Inv) ->
   Inv#inventory{ weapons = ordsets:add_element(V, Inv#inventory.weapons) }.

-spec add_armor (shr_armor:id(), type()) -> type().
add_armor (V, Inv) ->
   Inv#inventory{ armors = ordsets:add_element(V, Inv#inventory.armors) }.

-spec add_portrait (shr_portrait:id(), type()) -> type().
add_portrait (V, Inv) ->
   Inv#inventory{ portraits = ordsets:add_element(V, Inv#inventory.portraits) }.

-spec add_glyph_board (shr_glyph_board:id(), type()) -> type().
add_glyph_board (V, Inv) ->
   Inv#inventory
   {
      glyph_boards = ordsets:add_element(V, Inv#inventory.glyph_boards)
   }.

-spec add_glyph (shr_glyph:id(), type()) -> type().
add_glyph (V, Inv) ->
   Inv#inventory{ glyphs = ordsets:add_element(V, Inv#inventory.glyphs) }.


-spec ataxia_add_weapon
   (
      shr_weapon:id(),
      type()
   )
   -> {type(), ataxic:basic()}.
ataxia_add_weapon (V, Inv) ->
   CurrentWeapons = Inv#inventory.weapons,

   case ordsets:is_element(V, CurrentWeapons) of
      true -> {Inv, ataxic:current_value()};
      false ->
         {
            Inv#inventory{ weapons = ordsets:add_element(V, CurrentWeapons) },
            ataxic:update_field
            (
               get_weapons_field(),
               ataxic:apply_function
               (
                  ordsets,
                  add_element,
                  [ ataxic:constant(V), ataxic:current_value() ]
               )
            )
         }
   end.

-spec ataxia_add_armor
   (
      shr_armor:id(),
      type()
   )
   -> {type(), ataxic:basic()}.
ataxia_add_armor (V, Inv) ->
   CurrentArmors = Inv#inventory.armors,

   case ordsets:is_element(V, CurrentArmors) of
      true -> {Inv, ataxic:current_value()};
      false ->
         {
            Inv#inventory{ armors = ordsets:add_element(V, CurrentArmors) },
            ataxic:update_field
            (
               get_armors_field(),
               ataxic:apply_function
               (
                  ordsets,
                  add_element,
                  [ ataxic:constant(V), ataxic:current_value() ]
               )
            )
         }
   end.

-spec ataxia_add_portrait
   (
      shr_portrait:id(),
      type()
   )
   -> {type(), ataxic:basic()}.
ataxia_add_portrait (V, Inv) ->
   CurrentPortraits = Inv#inventory.portraits,

   case ordsets:is_element(V, CurrentPortraits) of
      true -> {Inv, ataxic:current_value()};
      false ->
         {
            Inv#inventory
            {
               portraits = ordsets:add_element(V, CurrentPortraits)
            },
            ataxic:update_field
            (
               get_portraits_field(),
               ataxic:apply_function
               (
                  ordsets,
                  add_element,
                  [ ataxic:constant(V), ataxic:current_value() ]
               )
            )
         }
   end.

-spec ataxia_add_glyph_board
   (
      shr_glyph_board:id(),
      type()
   )
   -> {type(), ataxic:basic()}.
ataxia_add_glyph_board (V, Inv) ->
   CurrentGlyphBoards = Inv#inventory.glyph_boards,

   case ordsets:is_element(V, CurrentGlyphBoards) of
      true -> {Inv, ataxic:current_value()};
      false ->
         {
            Inv#inventory
            {
               glyph_boards = ordsets:add_element(V, CurrentGlyphBoards)
            },
            ataxic:update_field
            (
               get_glyph_boards_field(),
               ataxic:apply_function
               (
                  ordsets,
                  add_element,
                  [ ataxic:constant(V), ataxic:current_value() ]
               )
            )
         }
   end.

-spec ataxia_add_glyph
   (
      shr_glyph:id(),
      type()
   )
   -> {type(), ataxic:basic()}.
ataxia_add_glyph (V, Inv) ->
   CurrentGlyphs = Inv#inventory.glyphs,

   case ordsets:is_element(V, CurrentGlyphs) of
      true -> {Inv, ataxic:current_value()};
      false ->
         {
            Inv#inventory{ glyphs = ordsets:add_element(V, CurrentGlyphs) },
            ataxic:update_field
            (
               get_glyphs_field(),
               ataxic:apply_function
               (
                  ordsets,
                  add_element,
                  [ ataxic:constant(V), ataxic:current_value() ]
               )
            )
         }
   end.


-spec default () -> type().
default () ->
   EmptySet = ordsets:new(),
   #inventory
   {
      weapons = EmptySet,
      armors = EmptySet,
      portraits = EmptySet,
      glyph_boards = EmptySet,
      glyphs = EmptySet
   }.

-spec allows_equipment (shr_equipment:either(), type()) -> boolean().
allows_equipment (Eq, Inv) ->
   Weapons = Inv#inventory.weapons,
   Glyphs = Inv#inventory.glyphs,

   (
      ordsets:is_element(shr_equipment:get_primary_weapon_id(Eq), Weapons)
      and ordsets:is_element(shr_equipment:get_secondary_weapon_id(Eq), Weapons)
      and
      ordsets:is_element
      (
         shr_equipment:get_armor_id(Eq),
         Inv#inventory.armors
      )
      and
      ordsets:is_element
      (
         shr_equipment:get_portrait_id(Eq),
         Inv#inventory.portraits
      )
      and
      ordsets:is_element
      (
         shr_equipment:get_glyph_board_id(Eq),
         Inv#inventory.glyph_boards
      )
      and
      lists:all
      (
         fun (G) -> ordsets:is_element(G, Glyphs) end,
         shr_equipment:get_glyph_ids(Eq)
      )
   ).

-spec add_equipment (shr_equipment:either(), type()) -> type().
add_equipment (Eq, Inv) ->
   Inv#inventory
   {
      weapons =
         ordsets:add_element
         (
            shr_equipment:get_primary_weapon_id(Eq),
            ordsets:add_element
            (
               shr_equipment:get_secondary_weapon_id(Eq),
               Inv#inventory.weapons
            )
         ),
      armors =
         ordsets:add_element
         (
            shr_equipment:get_armor_id(Eq),
            Inv#inventory.armors
         ),
      portraits =
         ordsets:add_element
         (
            shr_equipment:get_portrait_id(Eq),
            Inv#inventory.portraits
         ),
      glyph_boards =
         ordsets:add_element
         (
            shr_equipment:get_glyph_board_id(Eq),
            Inv#inventory.glyph_boards
         ),
      glyphs =
         lists:foldl
         (
            fun ordsets:add_element/2,
            Inv#inventory.glyph_boards,
            shr_equipment:get_glyph_ids(Eq)
         )
   }.

-spec ataxia_add_equipment
   (
      shr_equipment:either(),
      type()
   )
   -> {type(), ataxic:basic()}.
ataxia_add_equipment (Eq, Inv) ->
   {S0Inv, Ataxic0} =
      ataxia_add_weapon(shr_equipment:get_primary_weapon_id(Eq), Inv),

   {S1Inv, Ataxic1} =
      ataxia_add_weapon(shr_equipment:get_secondary_weapon_id(Eq), S0Inv),

   {S2Inv, Ataxic2} = ataxia_add_armor(shr_equipment:get_armor_id(Eq), S1Inv),

   {S3Inv, Ataxic3} =
      ataxia_add_glyph_board(shr_equipment:get_glyph_board_id(Eq), S2Inv),

   {S4Inv, Ataxic4s} =
      lists:foldl
      (
         fun (GlyphID, {PrevInv, PrevAtaxic}) ->
            {NewInv, NewAtaxic} = ataxia_add_glyph(GlyphID, PrevInv),
            {NewInv, [NewAtaxic|PrevAtaxic]}
         end,
         {S3Inv, []},
         shr_equipment:get_glyph_ids(Eq)
      ),

   {
      S4Inv,
      ataxic:optimize
      (
         ataxic:sequence([Ataxic0, Ataxic1, Ataxic2, Ataxic3|Ataxic4s])
      )
   }.

-spec decode (map()) -> type().
decode (Map) ->
   #inventory
   {
      weapons = ordsets:from_list(maps:get(?WEAPONS_FIELD, Map)),
      armors = ordsets:from_list(maps:get(?ARMORS_FIELD, Map)),
      portraits = ordsets:from_list(maps:get(?PORTRAITS_FIELD, Map)),
      glyph_boards = ordsets:from_list(maps:get(?GLYPH_BOARDS_FIELD, Map)),
      glyphs = ordsets:from_list(maps:get(?GLYPHS_FIELD, Map))
   }.

-spec encode (type()) -> {list({binary(), any()})}.
encode (Inv) ->
   {
      [
         {?WEAPONS_FIELD, ordsets:to_list(Inv#inventory.weapons)},
         {?ARMORS_FIELD, ordsets:to_list(Inv#inventory.armors)},
         {?PORTRAITS_FIELD, ordsets:to_list(Inv#inventory.portraits)},
         {?GLYPH_BOARDS_FIELD, ordsets:to_list(Inv#inventory.glyph_boards)},
         {?GLYPHS_FIELD, ordsets:to_list(Inv#inventory.glyphs)}
      ]
   }.

-spec get_weapons_field () -> non_neg_integer().
get_weapons_field () -> #inventory.weapons.

-spec get_armors_field () -> non_neg_integer().
get_armors_field () -> #inventory.armors.

-spec get_portraits_field () -> non_neg_integer().
get_portraits_field () -> #inventory.portraits.

-spec get_glyph_boards_field () -> non_neg_integer().
get_glyph_boards_field () -> #inventory.glyph_boards.

-spec get_glyphs_field () -> non_neg_integer().
get_glyphs_field () -> #inventory.glyphs.
