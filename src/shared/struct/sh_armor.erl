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
      get_coef/1,
      get_category/1
   ]
).

-export
(
   [
      random_id/0,
      from_id/1,
      apply_to_attributes/2
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

-spec get_coef (type()) -> float().
get_coef (Ar) -> Ar#armor.coef.

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
random_id () -> sh_roll:between(0, 24).

-spec apply_to_attributes
   (
      sh_attributes:type(),
      type()
   )
   -> sh_attributes:type().
apply_to_attributes (Att, Ar) ->
   Dexterity = sh_attributes:get_dexterity(Att),
   Speed = sh_attributes:get_speed(Att),
   Strength = sh_attributes:get_strength(Att),
   Mind = sh_attributes:get_mind(Att),
   Impact = erlang:ceil(-20.0 * Ar#armor.coef),
   Category = Ar#armor.category,

   case Category of
      kinetic -> sh_attributes:set_unsafe_mind((Mind - Impact), Att);
      leather -> sh_attributes:set_unsafe_dexterity((Dexterity - Impact), Att);
      chain ->
         sh_attributes:set_unsafe_dexterity
         (
            (Dexterity - Impact),
            sh_attributes:set_unsafe_speed((Speed - Impact), Att)
         );

      plate ->
         sh_attributes:set_unsafe_dexterity
         (
            (Dexterity - Impact),
            sh_attributes:set_unsafe_speed
            (
               (Speed - Impact),
               sh_attributes:set_unsafe_strength((Strength - Impact), Att)
            )
         )
   end.
