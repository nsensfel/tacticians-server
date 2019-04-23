-module(shr_character).

-define(NAME_FIELD, <<"nam">>).
-define(EQUIPMENT_FIELD, <<"eq">>).
-define(IS_USING_SECONDARY_FIELD, <<"sec">>).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record
(
   shr_char_ref,
   {
      name :: binary(),
      equipment :: shr_equipment:unresolved(),
      is_using_secondary :: boolean()
   }
).

-record
(
   shr_char,
   {
      name :: binary(),
      equipment :: shr_equipment:type(),
      is_using_secondary :: boolean(),
      statistics :: shr_statistics:type(),
      attributes :: shr_attributes:type(),
      extra_omnimods :: shr_omnimods:type(),
      omnimods :: shr_omnimods:type(),
      dirty :: boolean()
   }
).

-opaque type() :: #shr_char{}.
-opaque unresolved() :: #shr_char_ref{}.
-type either() :: (type() | unresolved()).

-export_type([type/0, unresolved/0, either/0]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Accessors
-export
(
   [
      get_name/1,
      get_equipment/1,
      get_attributes/1,
      get_statistics/1,
      get_active_weapon/1,
      get_inactive_weapon/1,
      get_omnimods/1,

      set_name/2,
      ataxia_set_name/2,

      set_equipment/2,
      ataxia_set_equipment/2,
      ataxia_set_equipment/3,

      set_extra_omnimods/2,

      switch_weapons/1,
      ataxia_switch_weapons/1,

      clean/1,
      is_dirty/1
   ]
).

-export
(
   [
      resolve/2,
      to_unresolved/1,
      encode/1,
      decode/1
   ]
).

-export
(
   [
      get_name_field/0,
      get_equipment_field/0,
      get_is_using_secondary_field/0
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Accessors
-spec get_name (either()) -> binary().
get_name (#shr_char{ name = R }) -> R;
get_name (#shr_char_ref{ name = R }) -> R.

-spec set_name
   (binary(), type()) -> type();
   (binary(), unresolved()) -> unresolved().
set_name (Name, Char) when is_record(Char, shr_char) ->
   Char#shr_char { name = Name };
set_name (Name, Char) when is_record(Char, shr_char_ref) ->
   Char#shr_char_ref{ name = Name }.

-spec ataxia_set_name
   (binary(), type()) -> {type(), ataxic:basic()};
   (binary(), unresolved()) -> {unresolved(), ataxic:basic()}.
ataxia_set_name (Name, Char) ->
   {
      set_name(Name, Char),
      ataxic:update_field
      (
         get_name_field(),
         ataxic:constant(Name)
      )
   }.

-spec get_equipment
   (type()) -> shr_equipment:type();
   (unresolved()) -> shr_equipment:unresolved().
get_equipment (#shr_char{ equipment = R }) -> R;
get_equipment (#shr_char_ref{ equipment = R }) -> R.

-spec set_equipment
   (shr_equipment:type(), type()) -> type();
   (shr_equipment:unresolved(), unresolved()) -> unresolved().
set_equipment (Eq, Char) when is_record(Char, shr_char) ->
   Char#shr_char
   {
      equipment = Eq,
      dirty = true
   };
set_equipment (EqRef, CharRef) when is_record(CharRef, shr_char_ref) ->
   CharRef#shr_char_ref{ equipment = EqRef }.

-spec ataxia_set_equipment
   (shr_equipment:type(), type()) -> {type(), ataxic:basic()};
   (
      shr_equipment:unresolved(),
      unresolved()
   )
   -> {unresolved(), ataxic:basic()}.
ataxia_set_equipment (Eq, Char) ->
   {
      set_equipment(Eq, Char),
      ataxic:update_field(get_equipment_field(), ataxic:constant(Eq))
   }.

-spec ataxia_set_equipment
   (shr_equipment:type(), ataxic:basic(), type()) -> {type(), ataxic:basic()};
   (
      shr_equipment:unresolved(),
      ataxic:basic(),
      unresolved()
   )
   -> {unresolved(), ataxic:basic()}.
ataxia_set_equipment (Eq, EqUpdate, Char) ->
   {
      set_equipment(Eq, Char),
      ataxic:update_field(get_equipment_field(), EqUpdate)
   }.

-spec switch_weapons
   (type()) -> type();
   (unresolved()) -> unresolved().
switch_weapons (Char) when is_record(Char, shr_char) ->
   Char#shr_char
   {
      is_using_secondary = (not Char#shr_char.is_using_secondary),
      dirty = true
   };
switch_weapons (Char) when is_record(Char, shr_char_ref) ->
   Char#shr_char_ref
   {
      is_using_secondary = (not Char#shr_char_ref.is_using_secondary)
   }.

-spec ataxia_switch_weapons
   (type()) -> {type(), ataxic:basic()};
   (unresolved()) -> {unresolved(), ataxic:basic()}.
ataxia_switch_weapons (Char) ->
   {
      switch_weapons(Char),
      ataxic:update_field
      (
         get_is_using_secondary_field(),
         ataxic:neg(ataxic:current_value())
      )
   }.

-spec get_active_weapon (either()) -> shr_weapon:type().
get_active_weapon (#shr_char{ is_using_secondary = B, equipment = E }) ->
   case B of
      true -> shr_equipment:get_secondary_weapon(E);
      false -> shr_equipment:get_primary_weapon(E)
   end;
get_active_weapon (#shr_char_ref{ is_using_secondary = B, equipment = E }) ->
   case B of
      true -> shr_equipment:get_secondary_weapon(E);
      false -> shr_equipment:get_primary_weapon(E)
   end.

-spec get_inactive_weapon (either()) -> shr_weapon:type().
get_inactive_weapon (#shr_char{ is_using_secondary = B, equipment = E }) ->
   case B of
      false -> shr_equipment:get_secondary_weapon(E);
      true -> shr_equipment:get_primary_weapon(E)
   end;
get_inactive_weapon (#shr_char_ref{ is_using_secondary = B, equipment = E }) ->
   case B of
      false -> shr_equipment:get_secondary_weapon(E);
      true -> shr_equipment:get_primary_weapon(E)
   end.

-spec get_attributes (type()) -> shr_attributes:type().
get_attributes (Char) -> Char#shr_char.attributes.

-spec get_statistics (type()) -> shr_statistics:type().
get_statistics (Char) -> Char#shr_char.statistics.

-spec get_omnimods (type()) -> shr_omnimods:type().
get_omnimods (Char) -> Char#shr_char.omnimods.

-spec set_extra_omnimods (shr_omnimods:type(), type()) -> type().
set_extra_omnimods (O, Char) ->
   Char#shr_char
   {
      extra_omnimods = O,
      dirty = true
   }.

-spec clean (type()) -> type().
clean (Char) when Char#shr_char.dirty ->
   Equipment = Char#shr_char.equipment,

   Omnimods =
      shr_omnimods:merge
      (
         shr_omnimods:merge
         (
            shr_glyph_board:get_omnimods_with_glyphs
            (
               shr_equipment:get_glyphs(Equipment),
               shr_equipment:get_glyph_board(Equipment)
            ),
            shr_armor:get_omnimods(shr_equipment:get_armor(Equipment))
         ),
         shr_omnimods:merge
         (
            shr_weapon:get_omnimods(get_active_weapon(Char)),
            Char#shr_char.extra_omnimods
         )
      ),

   Attributes =
      shr_omnimods:apply_to_attributes
      (
         shr_attributes:default(),
         Omnimods
      ),

   Statistics =
      shr_omnimods:apply_to_statistics
      (
         shr_statistics:new_raw(Attributes),
         Omnimods
      ),

   Char#shr_char
   {
      dirty = false,
      attributes = Attributes,
      statistics = Statistics,
      omnimods = Omnimods
   };
clean (Char) -> Char.

-spec is_dirty (type()) -> boolean().
is_dirty (Char) -> Char#shr_char.dirty.

-spec resolve (shr_omnimods:type(), unresolved()) -> type().
resolve (LocalOmnimods, CharRef) ->
   Attributes = shr_attributes:default(),

   #shr_char
   {
      name = CharRef#shr_char_ref.name,
      dirty = true,
      equipment = shr_equipment:resolve(CharRef#shr_char_ref.equipment),
      is_using_secondary = CharRef#shr_char_ref.is_using_secondary,
      statistics = shr_statistics:new_raw(Attributes),
      attributes = Attributes,
      omnimods = shr_omnimods:default(),
      extra_omnimods = LocalOmnimods
   }.

-spec to_unresolved (type()) -> unresolved().
to_unresolved (Char) ->
   #shr_char_ref
   {
      name = Char#shr_char.name,
      equipment = shr_equipment:to_unresolved(Char#shr_char.equipment),
      is_using_secondary = Char#shr_char.is_using_secondary
   }.

-spec decode (map()) -> unresolved().
decode (Map) ->
   #shr_char_ref
   {
      name = maps:get(?NAME_FIELD, Map),
      equipment = shr_equipment:decode(maps:get(?EQUIPMENT_FIELD, Map)),
      is_using_secondary = maps:get(?IS_USING_SECONDARY_FIELD, Map)
   }.

-spec encode (unresolved()) -> {list({binary(), any()})}.
encode (CharRef) ->
   {
      [
         {?NAME_FIELD, CharRef#shr_char_ref.name},
         {
            ?EQUIPMENT_FIELD,
            shr_equipment:encode(CharRef#shr_char_ref.equipment)
         },
         {?IS_USING_SECONDARY_FIELD, CharRef#shr_char_ref.is_using_secondary}
      ]
   }.

-spec get_name_field() -> non_neg_integer().
get_name_field () -> #shr_char_ref.name.

-spec get_equipment_field() -> non_neg_integer().
get_equipment_field () -> #shr_char_ref.equipment.

-spec get_is_using_secondary_field() -> non_neg_integer().
get_is_using_secondary_field () -> #shr_char_ref.is_using_secondary.
