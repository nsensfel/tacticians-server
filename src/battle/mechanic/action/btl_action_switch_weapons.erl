-module(btl_action_switch_weapons).
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec handle
   (
      btl_action:type(),
      btl_character_turn_update:type()
   )
   -> btl_character_turn_update:type().
handle (Action, S0Update) ->
   % TODO: assert actor is alive.
   ActorIX = btl_action:get_actor_index(Action),

   S0PerformSwitch = true,

   {S1PerformSwitch, S1Update} =
      btl_conditions:apply_to_character
      (
         ActorIX,
         ?CONDITION_TRIGGER_ABOUT_TO_SWITCH_WEAPONS,
         Action,
         S0PerformSwitch,
         S0Update
      ),

   {S2PerformSwitch, S2Update} =
      btl_conditions:apply_to_battle
      (
         ?CONDITION_TRIGGER_A_CHARACTER_IS_ABOUT_TO_SWITCH_WEAPONS,
         Action,
         S1PerformSwitch,
         S1Update
      ),

   case S2PerformSwitch of
      false -> S2Update;
      true ->
         S0Battle = btl_character_turn_update:get_battle(S2Update),
         {S0Actor, S1Battle} =
            btl_battle:get_resolved_character(ActorIX, S0Battle),

         S0BaseActor = btl_character:get_base_character(S0Actor),

         {S1BaseActor, BaseActorAtaxicUpdate} =
            shr_character:ataxia_switch_weapons(S0BaseActor),

         {S1Actor, ActorAtaxicUpdate} =
            btl_character:ataxia_set_base_character
            (
               S1BaseActor,
               BaseActorAtaxicUpdate,
               S0Actor
            ),

         {S2Battle, BattleAtaxicUpdate} =
            btl_battle:ataxia_set_character
            (
               ActorIX,
               S1Actor,
               ActorAtaxicUpdate,
               S1Battle
            ),

         TimelineItem =
            btl_turn_result:new_character_switched_weapons(ActorIX),

         S3Update =
            btl_character_turn_update:add_to_timeline(TimelineItem, S2Update),

         S4Update =
            btl_character_turn_update:ataxia_set_battle
            (
               S2Battle,
               BattleAtaxicUpdate,
               S3Update
            ),

         {_V0Nothing, S5Update} =
            btl_conditions:apply_to_character
            (
               ActorIX,
               ?CONDITION_TRIGGER_HAS_SWITCHED_WEAPONS,
               Action,
               none,
               S4Update
            ),

         {_V1Nothing, S6Update} =
            btl_conditions:apply_to_battle
            (
               ?CONDITION_TRIGGER_A_CHARACTER_HAS_SWITCHED_WEAPONS,
               Action,
               none,
               S5Update
            ),

         S6Update
   end.
