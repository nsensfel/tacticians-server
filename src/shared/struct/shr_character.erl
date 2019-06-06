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
      equipment_but_weapons_omnimods :: shr_omnimods:type(),
      extra_omnimods :: shr_omnimods:type(),
      omnimods :: shr_omnimods:type()
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
      ataxia_switch_weapons/1
   ]
).

-export
(
   [
      new/0,
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
-spec get_equipment_but_weapons_omnimods
   (
      shr_equipment:either()
   )
   -> shr_omnimods:type().
get_equipment_but_weapons_omnimods (Equipment) ->
   shr_omnimods:merge
   (
      shr_glyph_board:get_omnimods_with_glyphs
      (
         shr_equipment:get_glyphs(Equipment),
         shr_equipment:get_glyph_board(Equipment)
      ),
      shr_armor:get_omnimods(shr_equipment:get_armor(Equipment))
   ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec new () -> unresolved().
new () ->
   #shr_char_ref
   {
      name = <<"Unnamed Character">>,
      equipment = shr_equipment:default_unresolved(),
      is_using_secondary = false
   }.

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
   EquipmentButWeaponsOmnimods = get_equipment_but_weapons_omnimods(Eq),
   ActiveWeaponOmnimods =
      case Char#shr_char.is_using_secondary of
         false -> shr_weapon:get_omnimods(shr_equipment:get_primary_weapon(Eq));
         _ -> shr_weapon:get_omnimods(shr_equipment:get_secondary_weapon(Eq))
      end,

   NewOmnimods =
      shr_omnimods:merge
      (
         shr_omnimods:merge
         (
            EquipmentButWeaponsOmnimods,
            ActiveWeaponOmnimods
         ),
         Char#shr_char.extra_omnimods
      ),

   NewAttributes =
      shr_omnimods:apply_to_attributes
      (
         NewOmnimods,
         shr_attributes:default()
      ),

   NewStatistics =
      shr_omnimods:apply_to_statistics
      (
         NewOmnimods,
         shr_statistics:new_raw(NewAttributes)
      ),

   Char#shr_char
   {
      equipment = Eq,
      equipment_but_weapons_omnimods = EquipmentButWeaponsOmnimods,
      omnimods = NewOmnimods,
      attributes = NewAttributes,
      statistics = NewStatistics
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
   Eq = Char#shr_char.equipment,

   ActiveWeaponOmnimods =
      case Char#shr_char.is_using_secondary of
         true -> shr_weapon:get_omnimods(shr_equipment:get_primary_weapon(Eq));
         _ -> shr_weapon:get_omnimods(shr_equipment:get_secondary_weapon(Eq))
      end,

   NewOmnimods =
      shr_omnimods:merge
      (
         shr_omnimods:merge
         (
            Char#shr_char.equipment_but_weapons_omnimods,
            Char#shr_char.extra_omnimods
         ),
         ActiveWeaponOmnimods
      ),

   NewAttributes =
      shr_omnimods:apply_to_attributes
      (
         NewOmnimods,
         shr_attributes:default()
      ),

   NewStatistics =
      shr_omnimods:apply_to_statistics
      (
         NewOmnimods,
         shr_statistics:new_raw(NewAttributes)
      ),

   Char#shr_char
   {
      is_using_secondary = (not Char#shr_char.is_using_secondary),
      omnimods = NewOmnimods,
      attributes = NewAttributes,
      statistics = NewStatistics
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
   NewOmnimods =
      shr_omnimods:merge
      (
         shr_omnimods:merge
         (
            Char#shr_char.equipment_but_weapons_omnimods,
            shr_weapon:get_omnimods(get_active_weapon(Char))
         ),
         O
      ),

   NewAttributes =
      shr_omnimods:apply_to_attributes
      (
         NewOmnimods,
         shr_attributes:default()
      ),

   NewStatistics =
      shr_omnimods:apply_to_statistics
      (
         NewOmnimods,
         shr_statistics:new_raw(NewAttributes)
      ),

   Char#shr_char
   {
      extra_omnimods = O,
      omnimods = NewOmnimods,
      attributes = NewAttributes,
      statistics = NewStatistics
   }.

-spec resolve (shr_omnimods:type(), unresolved()) -> type().
resolve (LocalOmnimods, CharRef) ->
   Eq = shr_equipment:resolve(CharRef#shr_char_ref.equipment),

   EquipmentButWeaponsOmnimods = get_equipment_but_weapons_omnimods(Eq),

   NewOmnimods =
      shr_omnimods:merge
      (
         shr_omnimods:merge
         (
            EquipmentButWeaponsOmnimods,
            shr_weapon:get_omnimods(get_active_weapon(CharRef))
         ),
         LocalOmnimods
      ),

   NewAttributes =
      shr_omnimods:apply_to_attributes
      (
         NewOmnimods,
         shr_attributes:default()
      ),

   NewStatistics =
      shr_omnimods:apply_to_statistics
      (
         NewOmnimods,
         shr_statistics:new_raw(NewAttributes)
      ),

   #shr_char
   {
      name = CharRef#shr_char_ref.name,
      equipment_but_weapons_omnimods = EquipmentButWeaponsOmnimods,
      equipment = Eq,
      is_using_secondary = CharRef#shr_char_ref.is_using_secondary,
      statistics = NewStatistics,
      attributes = NewAttributes,
      omnimods = NewOmnimods,
      extra_omnimods = LocalOmnimods
   }.

-spec to_unresolved (either()) -> unresolved().
to_unresolved (Char) when is_record(Char, shr_char)->
   #shr_char_ref
   {
      name = Char#shr_char.name,
      equipment = shr_equipment:to_unresolved(Char#shr_char.equipment),
      is_using_secondary = Char#shr_char.is_using_secondary
   };
to_unresolved (CharRef) when is_record(CharRef, shr_char_ref) -> CharRef.

-spec decode (map()) -> unresolved().
decode (Map) ->
   #shr_char_ref
   {
      name = maps:get(?NAME_FIELD, Map),
      equipment = shr_equipment:decode(maps:get(?EQUIPMENT_FIELD, Map)),
      is_using_secondary = maps:get(?IS_USING_SECONDARY_FIELD, Map)
   }.

-spec encode (either()) -> {list({binary(), any()})}.
encode (Character) ->
   {
      [
         {?NAME_FIELD, get_name(Character)},
         {
            ?EQUIPMENT_FIELD,
            shr_equipment:encode(get_equipment(Character))
         },
         {
            ?IS_USING_SECONDARY_FIELD,
            (
               case Character of
                  #shr_char_ref{is_using_secondary = R} -> R;
                  #shr_char{is_using_secondary = R} -> R
               end
            )
         }
      ]
   }.

-spec get_name_field() -> non_neg_integer().
get_name_field () -> #shr_char_ref.name.

-spec get_equipment_field() -> non_neg_integer().
get_equipment_field () -> #shr_char_ref.equipment.

-spec get_is_using_secondary_field() -> non_neg_integer().
get_is_using_secondary_field () -> #shr_char_ref.is_using_secondary.
