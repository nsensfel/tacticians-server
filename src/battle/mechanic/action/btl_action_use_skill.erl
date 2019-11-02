-module(btl_action_use_skill).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-include("tacticians/conditions.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export
(
   [
      handle/2
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec pay_for_cast
   (
      non_neg_integer(),
      btl_character_turn_update:type()
   )
   -> {btl_character_turn_update:type(), shr_skill:type()}.
pay_for_cast (ActorIX, S0Update) ->
   S0Battle = btl_character_turn_update:get_battle(S0Update),
   S0Actor = btl_battle:get_character(ActorIX, S0Battle),
   BaseActor = btl_character:get_base_character(S0Actor),
   Equipment = shr_character:get_equipment(BaseActor),
   Skill = shr_equipment:get_skill(Equipment),

   SkillCost = shr_skill:get_cost(Skill),
   S0SkillPoints = btl_character:get_skill_points(S0Actor),
   S1SkillPoints = (S0SkillPoints - SkillCost),

   {S1Actor, ActorAtaxiaUpdate} =
      case (S1SkillPoints < 0) of
         true -> error({skill, points, S0SkillPoints, Skill});
         false ->
            btl_character:ataxia_set_skill_points(S1SkillPoints, S0Actor)
      end,

   {S1Battle, BattleAtaxiaUpdate} =
      btl_battle:ataxia_set_character
      (
         ActorIX,
         S1Actor,
         ActorAtaxiaUpdate,
         S0Battle
      ),

   S1Update =
      btl_character_turn_update:ataxia_set_battle
      (
         S1Battle,
         BattleAtaxiaUpdate,
         S0Update
      ),

   {S1Update, Skill}.

-spec cast_skill
   (
      btl_action:type(),
      btl_character_turn_update:type()
   )
   -> btl_character_turn_update:type().
cast_skill (Action, S0Update) ->
   ActorIX = btl_action:get_actor_index(Action),
   {S1Update, Skill} = pay_for_cast(ActorIX, S0Update),

   S2Update =
      erlang:apply
      (
         shr_skill:get_module(Skill),
         cast,
         [Skill, Action, S1Update]
      ),

   {none, S3Update} =
      btl_conditions:apply_to_character
      (
         ActorIX,
         ?CONDITION_TRIGGER_HAS_USED_THEIR_SKILL,
         Action,
         none,
         S2Update
      ),

   {none, S3Update} =
      btl_conditions:apply_to_battle
      (
         ?CONDITION_TRIGGER_A_CHARACTER_HAS_USED_THEIR_SKILL,
         Action,
         none,
         S2Update
      ),

   S3Update.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec handle
   (
      btl_action:type(),
      btl_character_turn_update:type()
   )
   -> btl_character_turn_update:type().
handle (S0Action, S0Update) ->
   ActorIX = btl_action:get_actor_index(S0Action),

   S0PerformAction = true,

   {{S1Action, S1PerformAction}, S1Update} =
      btl_conditions:apply_to_character
      (
         ActorIX,
         ?CONDITION_TRIGGER_ABOUT_TO_USE_THEIR_SKILL,
         none,
         {S0Action, S0PerformAction},
         S0Update
      ),

   {{S2Action, S2PerformAction}, S2Update} =
      btl_conditions:apply_to_battle
      (
         ?CONDITION_TRIGGER_A_CHARACTER_IS_ABOUT_TO_USE_THEIR_SKILL,
         none,
         {S1Action, S1PerformAction},
         S1Update
      ),

   case S2PerformAction of
      true -> cast_skill(S2Action, S2Update);
      false -> S2Update
   end.
