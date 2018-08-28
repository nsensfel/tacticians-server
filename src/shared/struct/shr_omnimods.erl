-module(shr_omnimods).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-type entry() :: {atom(), integer()}.
-type mods() :: dict:dict(atom(), integer()).

-record
(
   omnimods,
   {
      attmods :: mods(),
      stamods :: mods(),
      atkmods :: mods(),
      defmods :: mods()
   }
).

-opaque type() :: #omnimods{}.

-export_type([type/0, entry/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Accessors
-export
(
   [
      new/4,
      new_dirty/4
   ]
).

%%%% Modification
-export
(
   [
      merge/2,
      apply_coefficient/2
   ]
).

%%%% Access
-export
(
   [
      apply_to_attributes/2,
      apply_to_statistics/2,
      get_attack_damage/3
   ]
).

%%%% Export
-export
(
   [
      encode/1
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec cleanup_entry_list (list(entry())) -> list(entry()).
cleanup_entry_list (ModList) ->
   [First|Rem] = ModList,
   case First of
      {none, _} -> Rem;
      _ -> ModList
   end.

-spec apply_coefficient_to_mods (float(), mods()) -> mods().
apply_coefficient_to_mods (Coef, Mods) ->
   dict:map(fun (_Name, Val) -> shr_math_util:ceil(Coef * Val) end, Mods).

-spec merge_mods (mods(), mods()) -> mods().
merge_mods (ModsA, ModsB) ->
   dict:merge(fun (_Name, ValA, ValB) -> (ValA + ValB) end, ModsA, ModsB).

-spec encode_mods (mods()) -> list(any()).
encode_mods (Mods) ->
   lists:map
   (
      fun ({Name, Value}) ->
         {
            [
               {<<"t">>, Name},
               {<<"v">>, Value}
            ]
         }
      end,
      dict:to_list(Mods)
   ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Creation
-spec new
(
      list(entry()),
      list(entry()),
      list(entry()),
      list(entry())
   )
   -> type().
new (AttributeMods, StatisticMods, AttackMods, DefenseMods) ->
   #omnimods
   {
      attmods = dict:from_list(AttributeMods),
      stamods = dict:from_list(StatisticMods),
      atkmods = dict:from_list(AttackMods),
      defmods = dict:from_list(DefenseMods)
   }.

-spec new_dirty
(
      list(entry()),
      list(entry()),
      list(entry()),
      list(entry())
   )
   -> type().
new_dirty(AttributeMods, StatisticMods, AttackMods, DefenseMods) ->
   new
   (
      cleanup_entry_list(AttributeMods),
      cleanup_entry_list(StatisticMods),
      cleanup_entry_list(AttackMods),
      cleanup_entry_list(DefenseMods)
   ).

%%% Modification
-spec merge (type(), type()) -> type().
merge (OmniA, OmniB) ->
   OmniA#omnimods
   {
      attmods = merge_mods(OmniA#omnimods.attmods, OmniB#omnimods.attmods),
      stamods = merge_mods(OmniA#omnimods.stamods, OmniB#omnimods.stamods),
      atkmods = merge_mods(OmniA#omnimods.atkmods, OmniB#omnimods.atkmods),
      defmods = merge_mods(OmniA#omnimods.defmods, OmniB#omnimods.defmods)
   }.

-spec apply_coefficient (float(), type()) -> type().
apply_coefficient (Coef, Omnimods) ->
   Omnimods#omnimods
   {
      attmods = apply_coefficient_to_mods(Coef, Omnimods#omnimods.attmods),
      stamods = apply_coefficient_to_mods(Coef, Omnimods#omnimods.stamods),
      atkmods = apply_coefficient_to_mods(Coef, Omnimods#omnimods.atkmods),
      defmods = apply_coefficient_to_mods(Coef, Omnimods#omnimods.defmods)
   }.

%%% Access
-spec apply_to_attributes
   (
      type(),
      shr_attributes:type()
   )
   -> shr_attributes:type().
apply_to_attributes (Omnimods, Attributes) ->
   dict:fold
   (
      fun shr_attributes:apply_mod/3,
      Attributes,
      Omnimods#omnimods.attmods
   ).

-spec apply_to_statistics
   (
      type(),
      shr_statistics:type()
   )
   -> shr_statistics:type().
apply_to_statistics (Omnimods, Statistics) ->
   dict:fold
   (
      fun shr_statistics:apply_mod/3,
      Statistics,
      Omnimods#omnimods.attmods
   ).

-spec get_attack_damage (float(), type(), type()) -> non_neg_integer().
get_attack_damage (AttackModifier, AttackerOmnimods, DefenderOmnimods) ->
   AttackerOmnimodsAttmods = AttackerOmnimods#omnimods.atkmods,
   DefenderOmnimodsDefmods = DefenderOmnimods#omnimods.defmods,

   BaseDefense =
      case dict:find(base, DefenderOmnimodsDefmods) of
         {ok, BaseDefValue} -> BaseDefValue;
         _ -> 0
      end,

   Result =
      dict:fold
      (
         fun (Name, BaseDmg, CurrentResult) ->
            ModifiedDmg =
               (shr_math_util:ceil(BaseDmg * AttackModifier) - BaseDefense),
            case dict:find(Name, DefenderOmnimodsDefmods) of
               {ok, Def} when (Def >= ModifiedDmg) -> CurrentResult;
               {ok, Def} -> (CurrentResult + (ModifiedDmg - Def));
               _ -> (CurrentResult + ModifiedDmg)
            end
         end,
         0,
         AttackerOmnimodsAttmods
      ),

   Result.

%%% Export
-spec encode (type()) -> {list(any())}.
encode (Omnimods) ->
   {
      [
         {<<"attm">>, encode_mods(Omnimods#omnimods.attmods)},
         {<<"stam">>, encode_mods(Omnimods#omnimods.stamods)},
         {<<"atkm">>, encode_mods(Omnimods#omnimods.atkmods)},
         {<<"defm">>, encode_mods(Omnimods#omnimods.defmods)}
      ]
   }.
