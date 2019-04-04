-module(shr_inventory).

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

-export_type([type/0]).

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

      set_weapons/2,
      set_armors/2,
      set_portraits/2,
      set_glyph_boards/2,
      set_glyphs/2
   ]
).

%%%% Accessors
-export
(
   [
      default/0,
      allows_equipment/2,
      add_equipment/2
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

-spec set_weapons (ordsets:ordset(shr_weapon:id()), type()) -> type().
set_weapons (V, Inv) -> Inv#inventory{ weapons = V }.

-spec set_armors (ordsets:ordset(shr_armor:id()), type()) -> type().
set_armors (V, Inv) -> Inv#inventory{ armors = V }.

-spec set_portraits (ordsets:ordset(shr_portrait:id()), type()) -> type().
set_portraits (V, Inv) -> Inv#inventory{ portraits = V }.

-spec set_glyph_boards (ordsets:ordset(shr_glyph_board:id()), type()) -> type().
set_glyph_boards (V, Inv) -> Inv#inventory{ glyph_boards = V }.

-spec set_glyphs (ordsets:ordset(shr_glyph:id()), type()) -> type().
set_glyphs (V, Inv) -> Inv#inventory{ glyphs = V }.

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

-spec allows_equipment
   (
      (shr_equipment:type()|shr_equipment:unresolved()),
      type()
   )
   -> boolean().
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

-spec add_equipment
   (
      (shr_equipment:type()|shr_equipment:unresolved()),
      type()
   )
   -> type().
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
