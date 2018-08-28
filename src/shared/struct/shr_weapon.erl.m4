-module(shr_weapon).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-type id() :: non_neg_integer().

-type range_type() :: 'ranged' | 'melee'.
-type range_modifier() :: 'long' | 'short'.
-type damage_type() :: 'slash' | 'pierce' | 'blunt'.
-type damage_modifier() :: 'heavy' | 'light'.

-record
(
   weapon,
   {
      id :: id(),
      name :: binary(),
      range_type :: range_type(),
      range_mod :: range_modifier(),
      damage_type :: damage_type(),
      damage_mod :: damage_modifier(),
      coef :: float()
   }
).

-opaque type() :: #weapon{}.

-export_type([type/0, id/0]).
-export_type
(
   [
      range_type/0,
      range_modifier/0,
      damage_type/0,
      damage_modifier/0
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Accessors
-export
(
   [
      get_id/1,
      get_name/1,
      get_range_type/1,
      get_range_modifier/1,
      get_damage_type/1,
      get_damage_modifier/1,
      get_coefficient/1,
      get_ranges/1,
      get_damages/1
   ]
).

-export
(
   [
      random_id/0,
      from_id/1,
      can_parry/1,
      apply_to_attributes/2
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec ranges_of_type
   (
      range_type(),
      range_modifier()
   )
   -> {non_neg_integer(), non_neg_integer()}.
ranges_of_type (ranged, long) -> {2, 6};
ranges_of_type (ranged, short) -> {1, 4};
ranges_of_type (melee, long) -> {0, 2};
ranges_of_type (melee, short) -> {0, 1}.

-spec damages_of_type
   (
      range_type(),
      damage_modifier()
   )
   -> {non_neg_integer(), non_neg_integer()}.
damages_of_type (ranged, heavy) -> {15, 30};
damages_of_type (ranged, light) -> {10, 25};
damages_of_type (melee, heavy) -> {20, 35};
damages_of_type (melee, light) -> {15, 30}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Accessors
-spec get_id (type()) -> id().
get_id (Wp) -> Wp#weapon.id.

-spec get_name (type()) -> binary().
get_name (Wp) -> Wp#weapon.name.

-spec get_range_type (type()) -> range_type().
get_range_type (Wp) -> Wp#weapon.range_type.

-spec get_range_modifier (type()) -> range_modifier().
get_range_modifier (Wp) -> Wp#weapon.range_mod.

-spec get_damage_type (type()) -> damage_type().
get_damage_type (Wp) -> Wp#weapon.damage_type.

-spec get_damage_modifier (type()) -> damage_modifier().
get_damage_modifier (Wp) -> Wp#weapon.damage_mod.

-spec get_coefficient (type()) -> float().
get_coefficient (Wp) -> Wp#weapon.coef.

-spec get_ranges (type()) -> {non_neg_integer(), non_neg_integer()}.
get_ranges (Wp) ->
   ranges_of_type(Wp#weapon.range_type, Wp#weapon.range_mod).

-spec get_damages (type()) -> {non_neg_integer(), non_neg_integer()}.
get_damages (Wp) ->
   Coef = Wp#weapon.coef,
   {Min, Max} = damages_of_type(Wp#weapon.range_type, Wp#weapon.damage_mod),
   {shr_math_util:ceil(Min * Coef), shr_math_util:ceil(Max * Coef)}.

-spec can_parry (type()) -> boolean().
can_parry (Wp) -> (Wp#weapon.range_type == melee).

-spec from_id (id()) -> type().
m4_include(__MAKEFILE_DATA_DIR/weapon/global.m4.conf)m4_dnl
m4_include(__MAKEFILE_DATA_DIR/weapon/basic.m4d)m4_dnl
from_id (_) ->
   from_id(0).


-spec random_id () -> id().
random_id () -> shr_roll:between(0, 24).

-spec apply_to_attributes
   (
      type(),
      shr_attributes:type()
   )
   -> shr_attributes:type().
apply_to_attributes (Weapon, Attributes) ->
   Dexterity = shr_attributes:get_dexterity(Attributes),
   Speed = shr_attributes:get_speed(Attributes),
   RangeModifier = Weapon#weapon.range_mod,
   DamageModifier = Weapon#weapon.damage_mod,

   Impact = (20.0 * Weapon#weapon.coef),
   FullImpact = shr_math_util:ceil(Impact),
   QuarterImpact = shr_math_util:ceil(Impact / 4.0),

   ResultingDexterity =
      case RangeModifier of
         long -> (Dexterity - FullImpact);
         short -> (Dexterity - QuarterImpact)
      end,
   ResultingSpeed =
      case DamageModifier of
         heavy -> (Speed - FullImpact);
         light -> (Speed - QuarterImpact)
      end,

   S0Attributes = shr_attributes:set_unsafe_speed(ResultingSpeed, Attributes),
   S1Attributes =
      shr_attributes:set_unsafe_dexterity(ResultingDexterity, S0Attributes),

   S1Attributes.
