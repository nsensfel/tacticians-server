-module(sh_armor).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-opaque id() :: non_neg_integer().

-type category() :: 'kinetic' | 'leather' | 'chain' | 'plate'.

-record
(
   armor,
   {
      id :: id(),
      name :: binary(),
      category :: category(),
      coef :: float()
   }
).

-opaque type() :: #armor{}.

-export_type([type/0, id/0]).
-export_type ([category/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Accessors
-export
(
   [
      get_id/1,
      get_name/1,
      get_coefficient/1,
      get_category/1
   ]
).

-export
(
   [
      random_id/0,
      from_id/1,
      apply_to_attributes/2,
      get_resistance_to/2
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Accessors
-spec get_id (type()) -> id().
get_id (Ar) -> Ar#armor.id.

-spec get_name (type()) -> binary().
get_name (Ar) -> Ar#armor.name.

-spec get_coefficient (type()) -> float().
get_coefficient (Ar) -> Ar#armor.coef.

-spec get_category (type()) -> category().
get_category (Ar) -> Ar#armor.category.

-spec from_id (id()) -> type().
from_id (0) ->
   #armor{
      id = 0,
      name = <<"None">>,
      category = leather,
      coef = 0.0
   };
from_id (1) ->
   #armor{
      id = 1,
      name = <<"Last Meal's Pelts">>,
      category = leather,
      coef = 0.5
   };
from_id (2) ->
   #armor{
      id = 2,
      name = <<"Bits of Wall">>,
      category = plate,
      coef = 0.5
   };
from_id (3) ->
   #armor{
      id = 3,
      name = <<"Garden Fence">>,
      category = chain,
      coef = 0.5
   };
from_id (4) ->
   #armor{
      id = 4,
      name = <<"Morrigan's Pity">>,
      category = kinetic,
      coef = 0.5
   }.

-spec random_id () -> id().
random_id () -> sh_roll:between(0, 4).

-spec apply_to_attributes
   (
      type(),
      sh_attributes:type()
   )
   -> sh_attributes:type().
apply_to_attributes (Ar, Att) ->
   Constitution = sh_attributes:get_constitution(Att),
   Dexterity = sh_attributes:get_dexterity(Att),
   Speed = sh_attributes:get_speed(Att),
   Strength = sh_attributes:get_strength(Att),
   Mind = sh_attributes:get_mind(Att),
   Impact = sh_math_util:ceil(20.0 * Ar#armor.coef),
   HalfImpact = sh_math_util:ceil(10.0 * Ar#armor.coef),
   Category = Ar#armor.category,

   case Category of
      kinetic -> sh_attributes:set_unsafe_mind((Mind - Impact), Att);
      leather ->
         sh_attributes:set_unsafe_constitution
         (
            (Constitution - HalfImpact),
            sh_attributes:set_unsafe_dexterity((Dexterity - HalfImpact), Att)
         );

      chain ->
         sh_attributes:set_unsafe_constitution
         (
            (Constitution - HalfImpact),
            sh_attributes:set_unsafe_dexterity
            (
               (Dexterity - HalfImpact),
               sh_attributes:set_unsafe_speed((Speed - Impact), Att)
            )
         );

      plate ->
         sh_attributes:set_unsafe_constitution
         (
            (Constitution - HalfImpact),
            sh_attributes:set_unsafe_dexterity
            (
               (Dexterity - HalfImpact),
               sh_attributes:set_unsafe_speed
               (
                  (Speed - Impact),
                  sh_attributes:set_unsafe_strength((Strength - Impact), Att)
               )
            )
         )
   end.

-spec get_resistance_to (sh_weapon:damage_type(), type()) -> non_neg_integer().
get_resistance_to (DamageType, Armor) ->
   ArmorCategory = Armor#armor.category,
   BaseResistance =
      case {DamageType, ArmorCategory} of
         {slash, kinetic} -> 0.0;
         {slash, leather} -> 15.0;
         {slash, chain} -> 20.0;
         {slash, plate} -> 20.0;
         {blunt, kinetic} -> 15.0;
         {blunt, leather} -> 15.0;
         {blunt, chain} -> 15.0;
         {blunt, plate} -> 15.0;
         {pierce, kinetic} -> 15.0;
         {pierce, leather} -> 15.0;
         {pierce, chain} -> 15.0;
         {pierce, plate} -> 20.0
      end,

   ArmorCoefficient = Armor#armor.coef,
   ActualResistance = (ArmorCoefficient * BaseResistance),

   sh_math_util:ceil(ActualResistance).
