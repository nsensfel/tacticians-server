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
      default/0,
      new/3
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
-spec new (list(entry()), list(entry()), list(entry())) -> type().
new (StatisticMods, AttackMods, DefenseMods) ->
   #omnimods
   {
      stamods = dict:from_list(StatisticMods),
      atkmods = dict:from_list(AttackMods),
      defmods = dict:from_list(DefenseMods)
   }.

-spec default () -> type().
default () -> new([], [], []).

%%% Modification
-spec merge (type(), type()) -> type().
merge (OmniA, OmniB) ->
   OmniA#omnimods
   {
      stamods = merge_mods(OmniA#omnimods.stamods, OmniB#omnimods.stamods),
      atkmods = merge_mods(OmniA#omnimods.atkmods, OmniB#omnimods.atkmods),
      defmods = merge_mods(OmniA#omnimods.defmods, OmniB#omnimods.defmods)
   }.

-spec apply_coefficient (float(), type()) -> type().
apply_coefficient (Coef, Omnimods) ->
   Omnimods#omnimods
   {
      stamods = apply_coefficient_to_mods(Coef, Omnimods#omnimods.stamods),
      atkmods = apply_coefficient_to_mods(Coef, Omnimods#omnimods.atkmods),
      defmods = apply_coefficient_to_mods(Coef, Omnimods#omnimods.defmods)
   }.

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
      Omnimods#omnimods.stamods
   ).

-spec get_attack_damage (float(), type(), type()) -> non_neg_integer().
get_attack_damage (AttackModifier, AttackerOmnimods, DefenderOmnimods) ->
   AttackerOmnimodsAttmods = AttackerOmnimods#omnimods.atkmods,
   DefenderOmnimodsDefmods = DefenderOmnimods#omnimods.defmods,

   io:format("Attack!~n"),

   BaseDefense =
      case dict:find(base, DefenderOmnimodsDefmods) of
         {ok, BaseDefValue} -> BaseDefValue;
         _ -> 0
      end,

   io:format("Defender base defense: ~p~n", [BaseDefense]),

   Result =
      dict:fold
      (
         fun (Name, BaseDmg, CurrentResult) ->
            io:format("Precalc damage from ~p: ~p~n", [Name, BaseDmg]),
            NormDmg = max(0, BaseDmg),
            ModifiedDmg =
               (shr_math_util:ceil(NormDmg * AttackModifier) - BaseDefense),
            io:format("Actual attack damage from ~p: ~p~n", [Name, ModifiedDmg]),
            case dict:find(Name, DefenderOmnimodsDefmods) of
               {ok, Def} when (Def >= ModifiedDmg) ->
                  io:format
                  (
                     "Defender had ~p ~p armor, ignoring damage.~n",
                     [Def, Name]
                  ),
                  CurrentResult;

               {ok, Def} ->
                  DamageTaken = (ModifiedDmg - Def),
                  io:format
                  (
                     "Defender had ~p ~p armor, taking ~p damage.~n",
                     [Def, Name, DamageTaken]
                  ),
                  (CurrentResult + DamageTaken);
               _ ->
                  io:format
                  (
                     "Defender had no ~p armor, taking full damage.~n",
                     [Name]
                  ),
                  (CurrentResult + ModifiedDmg)
            end
         end,
         0,
         AttackerOmnimodsAttmods
      ),

   io:format("Defender took a total of ~p damage.~n", [Result]),

   Result.

%%% Export
-spec encode (type()) -> {list(any())}.
encode (Omnimods) ->
   {
      [
         {<<"stam">>, encode_mods(Omnimods#omnimods.stamods)},
         {<<"atkm">>, encode_mods(Omnimods#omnimods.atkmods)},
         {<<"defm">>, encode_mods(Omnimods#omnimods.defmods)}
      ]
   }.
