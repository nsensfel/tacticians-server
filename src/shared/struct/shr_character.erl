-module(shr_character).

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
      unchanging_omnimods :: shr_omnimods:type(),
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

      set_equipment/2,
      dirty_set_equipment/2,

      set_extra_omnimods/2,
      dirty_set_extra_omnimods/2,

      switch_weapons/1,
      dirty_switch_weapons/1
   ]
).

-export
(
   [
      resolve/2,
      to_unresolved/1
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
-spec refresh_omnimods (type()) -> type().
refresh_omnimods (Char) -> Char.

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

-spec get_equipment
   (type()) -> shr_equipment:type();
   (unresolved()) -> shr_equipment:unresolved().
get_equipment (#shr_char{ equipment = R }) -> R;
get_equipment (#shr_char_ref{ equipment = R }) -> R.

-spec switch_weapons
   (type()) -> type();
   (unresolved()) -> unresolved().
switch_weapons (Char) when is_record(Char, shr_char) ->
   refresh_omnimods
   (
      Char#shr_char
      {
         is_using_secondary = (not Char#shr_char.is_using_secondary)
      }
   );
switch_weapons (Char) when is_record(Char, shr_char_ref) ->
   Char#shr_char_ref
   {
      is_using_secondary = (not Char#shr_char_ref.is_using_secondary)
   }.

-spec dirty_switch_weapons
   (type()) -> type();
   (unresolved()) -> unresolved().
dirty_switch_weapons (Char) when is_record(Char, shr_char) ->
   Char#shr_char
   {
      is_using_secondary = (not Char#shr_char.is_using_secondary)
   };
dirty_switch_weapons (Char) when is_record(Char, shr_char_ref) ->
   Char#shr_char_ref
   {
      is_using_secondary = (not Char#shr_char_ref.is_using_secondary)
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
   refresh_omnimods(Char#shr_char{ extra_omnimods = O }).

-spec dirty_set_extra_omnimods (shr_omnimods:type(), type()) -> type().
dirty_set_extra_omnimods (O, Char) -> Char#shr_char{ extra_omnimods = O }.

-spec resolve (shr_omnimods:type(), unresolved()) -> type().
resolve (LocalOmnimods, CharRef) ->
   ResolvedEquipment = shr_equipment:resolve(CharRef#shr_char_ref.equipment),
   UsingSecondary = CharRef#shr_char_ref.is_using_secondary,

   UnchangingOmnimods =
      shr_omnimods:merge
      (
         shr_glyph_board:get_omnimods_with_glyphs
         (
            shr_equipment:get_glyphs(ResolvedEquipment),
            shr_equipment:get_glyph_board(ResolvedEquipment)
         ),
         get_armor(CharRef)
      ),

   Omnimods =
      shr_omnimods:merge
      (
         UnchangingOmnimods,
         shr_omnimods:merge
         (
            get_active_weapon(CharRef),
            LocalOmnimods
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

   #shr_char
   {
      name = CharRef#shr_char_ref.name,
      equipment = ResolvedEquipment,
      is_using_secondary = UsingSecondary,
      statistics = Statistics,
      attributes = Attributes,
      unchanging_omnimods = UnchangingOmnimods,
      omnimods = Omnimods
   }.


-spec get_name_field() -> non_neg_integer().
get_name_field () -> #shr_char_ref.name.
-spec get_equipment_field() -> non_neg_integer().
get_equipment_field () -> #shr_char_ref.equipment.
-spec get_is_using_secondary_field() -> non_neg_integer().
get_is_using_secondary_field () -> #shr_char_ref.is_using_secondary.
