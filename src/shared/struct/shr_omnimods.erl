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
-export
(
   [
      new/4,
      new_dirty/4
   ]
).

%%%% Accessors
-export
(
   [
      merge/2,
      apply_coefficient/2
   ]
).

-export
(
   [
      apply_to_attributes/3,
      apply_to_statistics/3,
      get_attack_damage/3
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
   dict:map(fun ({_Name, Val}) -> shr_util:ceil(Coef * Val) end, Mods).

-spec merge_mods (mods(), mods()) -> mods().
merge_mods (ModsA, ModsB) ->
   dict:merge(fun (_Name, ValA, ValB) -> (ValA + ValB) end, ModsA, ModsB).

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

%%% Application
-spec apply_to_attributes
   (
      fun((atom(), integer(), shr_attributes:type()) -> shr_attributes:type()),
      type(),
      shr_attributes:type()
   )
   -> shr_attributes:type().
apply_to_attributes (UpdateFun, Omnimods, Attributes) ->
   dict:fold
   (
      UpdateFun,
      Attributes,
      Omnimods#omnimods.attmods
   ).

-spec apply_to_statistics
   (
      fun((atom(), integer(), shr_statistics:type()) -> shr_statistics:type()),
      type(),
      shr_statistics:type()
   )
   -> shr_statistics:type().
apply_to_statistics (UpdateFun, Omnimods, Statistics) ->
   dict:fold
   (
      UpdateFun,
      Statistics,
      Omnimods#omnimods.attmods
   ).

-spec get_attack_damage (float(), type(), type()) -> non_neg_integer().
get_attack_damage (AttackModifier, AttackerOmnimods, DefenderOmnimods) ->
   AttackerOmnimodsAttmods = AttackerOmnimods#omnimods.atkmods,
   DefenderOmnimodsDefmods = DefenderOmnimods#omnimods.defmods,

   S0Calc =
      apply_coefficient_to_mods(-1*AttackModifier, AttackerOmnimodsAttmods),
   S1Calc = merge_mods(DefenderOmnimodsDefmods, S0Calc),

   Result =
      dict:fold
      (
         fun (_Name, Val, CurrResult) ->
            case (Val > 0) of
               true -> (CurrResult + Val);
               _ -> CurrResult
            end
         end,
         0,
         S1Calc
      ),

   Result.
