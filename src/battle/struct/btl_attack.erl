-module(btl_attack).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-type category() :: ('first' | 'second' | 'counter').
-type precision() :: ('misses' | 'grazes' | 'hits').

-record
(
   attack,
   {
      category :: category(),
      precision :: precision(),
      is_critical :: boolean(),
      is_parry :: boolean(),
      damage :: non_neg_integer()
   }
).

-opaque type() :: #attack{}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export_type([type/0, category/0]).

-export
(
   [
      new/5
   ]
).

-export
(
   [
      encode/1
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec encode_category (category()) -> binary().
encode_category (first) -> <<"f">>;
encode_category (counter) -> <<"c">>;
encode_category (second) -> <<"s">>.

-spec encode_precision (precision()) -> binary().
encode_precision (hits) -> <<"h">>;
encode_precision (grazes) -> <<"g">>;
encode_precision (misses) -> <<"m">>.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec new
   (
      category(),
      float(),
      float(),
      boolean(),
      non_neg_integer()
   )
   -> type().
new (Category, PrecisionModifier, CriticalModifier, IsParry, Damage) ->
   #attack
   {
      category = Category,
      precision =
         case PrecisionModifier of
            1.0 -> hits;
            0.5 -> grazes;
            0.0 -> misses
         end,
      is_critical = (CriticalModifier > 0.0),
      is_parry = IsParry,
      damage = Damage
   }.

-spec encode (type()) -> {list(any())}.
encode (Attack) ->
   Category = Attack#attack.category,
   Precision = Attack#attack.precision,
   IsCritical = Attack#attack.is_critical,
   IsParry = Attack#attack.is_parry,
   Damage = Attack#attack.damage,

   {
      [
         {<<"ord">>, encode_category(Category)},
         {<<"pre">>, encode_precision(Precision)},
         {<<"cri">>, IsCritical},
         {<<"par">>, IsParry},
         {<<"dmg">>, Damage}
      ]
   }.
