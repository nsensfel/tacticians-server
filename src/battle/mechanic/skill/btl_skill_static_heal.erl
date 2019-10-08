-module(btl_skill_static_heal).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-include("tacticians/skills.hrl")
-include("tacticians/conditions.hrl")

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export
(
   [
      cast/5
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec cast_logic
   (
      non_neg_integer(),
      non_neg_integer(),
      btl_character_turn_update:type()
   )
   -> btl_character_turn_update:type().
cast_logic (TargetIX, Amount, S0Update) ->
   Healing =
      btl_condition:new
      (
         ?CONDITION_EFFECT_HEAL,
         [],
         -1,
         1,
         {TargetIX, HealAmount}
      ),

   {_S1HealindAndUpdate, S1Update} =
      btl_cond_heal:apply
      (
         Healing,
         S0Update
      ),

   % TODO: Add a btl_turn_result showing the heal to S1Update.

   S1Update.

-spec group_cast_logic
   (
      list(non_neg_integer()),
      non_neg_integer(),
      non_neg_integer(),
      btl_character_turn_update:type()
   )
   -> btl_character_turn_update:type().
group_cast_logic (TargetIXs, MaxTargets, Amount, S0Update) ->
   case (length(TargetIXs) > MaxTargets) of
      true -> error({skill, target, TargetIXs});
      false -> ok
   end,

   lists:foldl
   (
      fun (TargetIX, S1Update) ->
         cast_logic(Amount, TargetIX, S1Update)
      end,
      S0Update,
      TargetIXs
   ).

-spec player_cast_logic
   (
      non_neg_integer(),
      non_neg_integer(),
      btl_character_turn_update:type()
   )
   -> btl_character_turn_update:type().
player_cast_logic (TargetIX, Amount, S0Update) ->
   Characters = todo, %TODO
   TargetRef = todo, %TODO
   TargetPlayerIX = todo, %TODO

   % TODO:
   % apply `cast_logic(Amount, TargetIX, S1Update)` on all characters whose
   % PlayerIX == TargetPlayerIX.
   S0Update.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec cast
   (
      shr_skill:variant(),
      non_neg_integer(),
      list(non_neg_integer()),
      list(shr_location:type()),
      btl_character_turn_update:type()
   )
   -> btl_character_turn_update:type().
cast (?SKILL_VARIANT_ALPHA_0, _UserIX, [TargetIX], _Locations, S0Update) ->
   cast_logic(20, TargetIX, S0Update);
cast (?SKILL_VARIANT_ALPHA_1, _UserIX, [TargetIX], _Locations, S0Update) ->
   cast_logic(40, TargetIX, S0Update);
cast (?SKILL_VARIANT_ALPHA_2, _UserIX, [TargetIX], _Locations, S0Update) ->
   cast_logic(60, TargetIX, S0Update);
cast (?SKILL_VARIANT_ALPHA_3, _UserIX, [TargetIX], _Locations, S0Update) ->
   cast_logic(80, TargetIX, S0Update);
cast (?SKILL_VARIANT_ALPHA_4, _UserIX, [TargetIX], _Locations, S0Update) ->
   cast_logic(100, TargetIX, S0Update);
cast (?SKILL_VARIANT_PHI_0, _UserIX, TargetIXs, _Locations, S0Update) ->
   group_cast_logic(TargetIXs, 2, 20, S0Update);
cast (?SKILL_VARIANT_PHI_1, _UserIX, TargetIXs, _Locations, S0Update) ->
   group_cast_logic(TargetIXs, 2, 60, S0Update);
cast (?SKILL_VARIANT_PHI_2, _UserIX, TargetIXs, _Locations, S0Update) ->
   group_cast_logic(TargetIXs, 2, 100, S0Update);
cast (?SKILL_VARIANT_PHI_3, _UserIX, TargetIXs, _Locations, S0Update) ->
   group_cast_logic(TargetIXs, 4, 20, S0Update);
cast (?SKILL_VARIANT_PHI_4, _UserIX, TargetIXs, _Locations, S0Update) ->
   group_cast_logic(TargetIXs, 4, 60, S0Update);
cast (?SKILL_VARIANT_PHI_5, _UserIX, TargetIXs, _Locations, S0Update) ->
   group_cast_logic(TargetIXs, 4, 100, S0Update);
cast (?SKILL_VARIANT_PHI_6, _UserIX, TargetIXs, _Locations, S0Update) ->
   group_cast_logic(TargetIXs, 8, 20, S0Update);
cast (?SKILL_VARIANT_PHI_7, _UserIX, TargetIXs, _Locations, S0Update) ->
   group_cast_logic(TargetIXs, 8, 60, S0Update);
cast (?SKILL_VARIANT_PHI_8, _UserIX, TargetIXs, _Locations, S0Update) ->
   group_cast_logic(TargetIXs, 8, 100, S0Update);
cast (?SKILL_VARIANT_PSI_0, _UserIX, [TargetIX], _Locations, S0Update) ->
   player_cast_logic(TargetIX, 20, S0Update);
cast (?SKILL_VARIANT_PSI_1, _UserIX, [TargetIX], _Locations, S0Update) ->
   player_cast_logic(TargetIX, 40, S0Update);
cast (?SKILL_VARIANT_PSI_2, _UserIX, [TargetIX], _Locations, S0Update) ->
   player_cast_logic(TargetIX, 60, S0Update);
cast (?SKILL_VARIANT_PSI_3, _UserIX, [TargetIX], _Locations, S0Update) ->
   player_cast_logic(TargetIX, 80, S0Update).
