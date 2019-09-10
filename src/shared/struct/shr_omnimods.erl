-module(shr_omnimods).

-include("tacticians/attributes.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-type attribute_entry() :: {shr_attributes:enum(), integer()}.
-type attribute_mods() :: dict:dict(shr_attributes:enum(), integer()).

-type damage_type_entry() :: {shr_damage_type:type(), integer()}.
-type damage_type_mods() :: dict:dict(shr_damage_type:type(), integer()).

-type entry() :: (attribute_entry() | damage_type_entry()).
-type mods() :: (attribute_mods() | damage_type_mods()).

-record
(
   omnimods,
   {
      attmods :: attribute_mods(),
      atkmods :: damage_type_mods(),
      defmods :: damage_type_mods()
   }
).

-opaque type() :: #omnimods{}.

-export_type
(
   [
      type/0,
      attribute_entry/0,
      damage_type_entry/0,
      entry/0
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Accessors
-export
(
   [
      new/0,
      new/3
   ]
).

%%%% Modification
-export
(
   [
      merge/2,
      apply_coefficient/2,
      set_attribute_modifiers/2,
      set_attack_modifiers/2,
      set_defense_modifiers/2,
      mod_attribute_modifier/3,
      mod_attack_modifier/3,
      mod_defense_modifier/3
   ]
).

%%%% Access
-export
(
   [
      apply_to_attributes/2,
      get_attack_damage/3,
      get_attribute_modifier/2,
      get_attack_modifier/2,
      get_defense_modifier/2,
      get_attribute_modifiers/1,
      get_attack_modifiers/1,
      get_defense_modifiers/1
   ]
).

%%%% Export
-export
(
   [
      encode/1,
      export/1
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec apply_coefficient_to_mods
   (float(), attribute_mods()) -> attribute_mods();
   (float(), damage_type_mods()) -> damage_type_mods().
apply_coefficient_to_mods (Coef, Mods) ->
   dict:map(fun (_Name, Val) -> shr_math_util:ceil(Coef * Val) end, Mods).

-spec merge_mods
   (attribute_mods(), attribute_mods()) -> attribute_mods();
   (damage_type_mods(), damage_type_mods()) -> damage_type_mods().
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

-spec mod_list_to_string_list (mods()) -> list().
mod_list_to_string_list (Mods) ->
   (
      "__MOD_LIST("
      ++
      lists:map
      (
         fun ({Name, Value}) ->
            io_lib:format("__MOD_~p(~p),", [Name, Value])
         end,
         Mods
      )
      ++
      ")"
   ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Creation
-spec new
   (
      list(attribute_entry()),
      list(damage_type_entry()),
      list(damage_type_entry())
   )
   -> type().
new (AttributeMods, AttackMods, DefenseMods) ->
   #omnimods
   {
      attmods = dict:from_list(AttributeMods),
      atkmods = dict:from_list(AttackMods),
      defmods = dict:from_list(DefenseMods)
   }.

-spec new () -> type().
new () -> new([], [], []).

%%% Modification
-spec merge (type(), type()) -> type().
merge (OmniA, OmniB) ->
   OmniA#omnimods
   {
      attmods = merge_mods(OmniA#omnimods.attmods, OmniB#omnimods.attmods),
      atkmods = merge_mods(OmniA#omnimods.atkmods, OmniB#omnimods.atkmods),
      defmods = merge_mods(OmniA#omnimods.defmods, OmniB#omnimods.defmods)
   }.

-spec apply_coefficient (float(), type()) -> type().
apply_coefficient (Coef, Omnimods) ->
   Omnimods#omnimods
   {
      attmods = apply_coefficient_to_mods(Coef, Omnimods#omnimods.attmods),
      atkmods = apply_coefficient_to_mods(Coef, Omnimods#omnimods.atkmods),
      defmods = apply_coefficient_to_mods(Coef, Omnimods#omnimods.defmods)
   }.

-spec set_attribute_modifiers (list(attribute_entry()), type()) -> type().
set_attribute_modifiers (NewAttributeModifiers, Omnimods) ->
   Omnimods#omnimods
   {
      attmods = dict:from_list(NewAttributeModifiers)
   }.

-spec set_defense_modifiers (list(damage_type_entry()), type()) -> type().
set_defense_modifiers (NewDefenseModifiers, Omnimods) ->
   Omnimods#omnimods
   {
      defmods = dict:from_list(NewDefenseModifiers)
   }.

-spec set_attack_modifiers (list(damage_type_entry()), type()) -> type().
set_attack_modifiers (NewAttackModifiers, Omnimods) ->
   Omnimods#omnimods
   {
      atkmods = dict:from_list(NewAttackModifiers)
   }.

-spec mod_attribute_modifier
   (
      shr_attributes:enum(),
      integer(),
      type()
   )
   -> type().
mod_attribute_modifier (Attribute, Mod, Omnimods) ->
   Omnimods#omnimods
   {
      attmods =
         dict:update
         (
            Attribute,
            fun (Current) -> (Current + Mod) end,
            Mod,
            Omnimods#omnimods.attmods
         )
   }.

-spec mod_attack_modifier
   (
      shr_damage_type:type(),
      integer(),
      type()
   )
   -> type().
mod_attack_modifier (DamageType, Mod, Omnimods) ->
   Omnimods#omnimods
   {
      atkmods =
         dict:update
         (
            DamageType,
            fun (Current) -> (Current + Mod) end,
            Mod,
            Omnimods#omnimods.atkmods
         )
   }.

-spec mod_defense_modifier
   (
      shr_damage_type:type(),
      integer(),
      type()
   )
   -> type().
mod_defense_modifier (DamageType, Mod, Omnimods) ->
   Omnimods#omnimods
   {
      defmods =
         dict:update
         (
            DamageType,
            fun (Current) -> (Current + Mod) end,
            Mod,
            Omnimods#omnimods.defmods
         )
   }.

%%%% Access
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

-spec get_attack_damage (float(), type(), type()) -> non_neg_integer().
get_attack_damage (AttackMultiplier, AttackerOmnimods, DefenderOmnimods) ->
   AttackerOmnimodsAtkmods = AttackerOmnimods#omnimods.atkmods,
   DefenderOmnimodsDefmods = DefenderOmnimods#omnimods.defmods,

   io:format("Attack!~n"),

   Result =
      dict:fold
      (
         fun (Name, BaseDmg, CurrentResult) ->
            io:format("Precalc damage from ~p: ~p~n", [Name, BaseDmg]),
            NormDmg = max(0, BaseDmg),
            ModifiedDmg = (shr_math_util:ceil(NormDmg * AttackMultiplier)),

            io:format
            (
               "Actual attack damage from ~p: ~p~n",
               [Name, ModifiedDmg]
            ),

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
         AttackerOmnimodsAtkmods
      ),

   io:format("Defender took a total of ~p damage.~n", [Result]),

   Result.

-spec get_attribute_modifier (shr_attributes:enum(), type()) -> integer().
get_attribute_modifier (Attribute, Omnimods) ->
   case dict:find(Attribute, Omnimods#omnimods.attmods) of
      {ok, Value} -> Value;
      error -> 0
   end.

-spec get_attack_modifier (shr_damage_type:type(), type()) -> integer().
get_attack_modifier (DamageType, Omnimods) ->
   case dict:find(DamageType, Omnimods#omnimods.atkmods) of
      {ok, Value} -> Value;
      error -> 0
   end.

-spec get_defense_modifier (shr_damage_type:type(), type()) -> integer().
get_defense_modifier (DamageType, Omnimods) ->
   case dict:find(DamageType, Omnimods#omnimods.defmods) of
      {ok, Value} -> Value;
      error -> 0
   end.

-spec get_attribute_modifiers (type()) -> list(attribute_entry()).
get_attribute_modifiers (Omnimods) -> dict:to_list(Omnimods#omnimods.attmods).

-spec get_attack_modifiers (type()) -> list(damage_type_entry()).
get_attack_modifiers (Omnimods) -> dict:to_list(Omnimods#omnimods.atkmods).

-spec get_defense_modifiers (type()) -> list(damage_type_entry()).
get_defense_modifiers (Omnimods) -> dict:to_list(Omnimods#omnimods.defmods).


%%% Export
-spec encode (type()) -> {list(any())}.
encode (Omnimods) ->
   {
      [
         {<<"attm">>, encode_mods(Omnimods#omnimods.attmods)},
         {<<"atkm">>, encode_mods(Omnimods#omnimods.atkmods)},
         {<<"defm">>, encode_mods(Omnimods#omnimods.defmods)}
      ]
   }.

-spec export (type()) -> list().
export (Omnimods) ->
   (
      mod_list_to_string_list(dict:to_list(Omnimods#omnimods.attmods))
      ++ "\n"
      ++ mod_list_to_string_list(dict:to_list(Omnimods#omnimods.atkmods))
      ++ "\n"
      ++ mod_list_to_string_list(dict:to_list(Omnimods#omnimods.defmods))
   ).
