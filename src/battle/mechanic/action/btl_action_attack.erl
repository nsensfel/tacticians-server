-module(btl_action_attack).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% FIXME: Luck should be modifiable by conditions, because they may be chance
% related (e.g. 25% chance of healing after a hit).
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
-spec apply_conditions
   (
      shr_condition:context(A, B),
      btl_character:type(),
      btl_character_turn_update:type()
   )
   -> {shr_condition:context(A, B), btl_character_turn_update:type()}.
apply_conditions
(
   Context = {Trigger, _ReadOnlyContext, _VolatileContext},
   Actor,
   S0Update
) ->
   {LastContext, S1Update} =
      btl_condition:recursive_apply
      (
         btl_character:get_conditions_on(Trigger, Actor),
         Context,
         S0Update
      ),

   {LastContext, S1Update}.

-spec roll_for_precision
   (
      btl_character:type(),
      integer(),
      btl_character:type(),
      integer()
   )
   -> {btl_attack:precision(), integer(), integer()}.
roll_for_precision (Actor, ActorLuck, Target, TargetLuck) ->
   TargetDodgeChance =
      shr_attributes:get_dodge_chance
      (
         shr_character:get_attributes
         (
            btl_character:get_base_character(Target)
         )
      ),
   ActorAccuracy =
      shr_attributes:get_accuracy
      (
         shr_character:get_attributes
         (
            btl_character:get_base_character(Actor)
         )
      ),

   MissChance = max(0, (TargetDodgeChance - ActorAccuracy)),

   {Roll, _IsSuccess, PositiveModifier, NegativeModifier} =
      shr_roll:percentage_with_luck(MissChance, TargetLuck),

   {
      (
         if
            (Roll =< MissChance) -> misses;
            (Roll =< (MissChance * 2)) -> grazes;
            true -> hits
         end
      ),
      (ActorLuck + NegativeModifier), % Negative effects are for Actor.
      (TargetLuck + PositiveModifier) % Positive effects are for Target.
   }.

-spec roll_for_critical_hit
   (
      btl_character:type(),
      integer(),
      btl_character:type(),
      integer()
   )
   -> {boolean(), integer(), integer()}.
roll_for_critical_hit (Actor, ActorLuck, _Target, TargetLuck) ->
   ActorCriticalHitChance =
      shr_attributes:get_critical_hit_chance
      (
         shr_character:get_attributes
         (
            btl_character:get_base_character(Actor)
         )
      ),

   {_Roll, IsSuccess, PositiveModifier, NegativeModifier} =
      shr_roll:percentage_with_luck(ActorCriticalHitChance, ActorLuck),

   {
      IsSuccess,
      (ActorLuck + PositiveModifier), % Positive effects are for Actor
      (TargetLuck + NegativeModifier) % Negative effects are for Target
   }.

-spec roll_for_parry
   (
      btl_character:type(),
      integer(),
      btl_character:type(),
      integer()
   )
   -> {boolean(), integer(), integer()}.
roll_for_parry (_Actor, ActorLuck, Target, TargetLuck) ->
   TargetParryChance =
      shr_attributes:get_critical_hit_chance
      (
         shr_character:get_attributes
         (
            btl_character:get_base_character(Target)
         )
      ),

   {_Roll, IsSuccess, PositiveModifier, NegativeModifier} =
      shr_roll:percentage_with_luck(TargetParryChance, TargetLuck),

   {
      IsSuccess,
      (ActorLuck + NegativeModifier), % Negative effects are for Actor
      (TargetLuck + PositiveModifier) % Positive effects are for Target
   }.

-spec can_perform_hit
   (
      btl_character:type(),
      btl_character:type()
   )
   -> {boolean(), boolean()}.
can_perform_hit (Actor, Target) ->
   ActorWeapon =
      shr_character:get_active_weapon
      (
         btl_character:get_base_character(Actor)
      ),

   ActorMinimumRange = shr_weapon:get_minimum_range(ActorWeapon),
   ActorMaximumRange = shr_weapon:get_maximum_range(ActorWeapon),

   RequiredRange =
      shr_location:dist
      (
         btl_character:get_location(Actor),
         btl_character:get_location(Target)
      ),

   {
      (
         (RequiredRange =< ActorMaximumRange)
         and (RequiredRange >= ActorMinimumRange)
      ),
      (btl_character:get_is_alive(Actor) and btl_character:get_is_alive(Target))
   }.

-spec apply_mirror_conditions
   (
      boolean(),
      shr_condition:trigger(),
      shr_condition:trigger(),
      btl_action:type(),
      {any(), VolatileContext},
      btl_character_turn_update:type()
   )
   -> {VolatileContext, btl_character_turn_update:type()}.
apply_mirror_conditions
(
   false,
   OwnTriggerName,
   OtherTriggerName,
   Action,
   {ReadOnlyContext, S0VolatileContext},
   S0Update
) ->
   CharacterIX = btl_action:get_actor_index(Action),
   S0Battle = btl_character_turn_update:get_battle(S0Update),
   {Character, S1Battle} =
      btl_battle:get_resolved_character(CharacterIX, S0Battle),

   S1Update = btl_character_turn_update:set_battle(S1Battle, S0Update),

   {{_TriggerName, _ReadOnlyContext, S1VolatileContext}, S2Update} =
      apply_conditions
      (
         {OwnTriggerName, ReadOnlyContext, S0VolatileContext},
         Character,
         S1Update
      ),

   TargetCharacterIX = btl_action:get_target_index(Action),
   S2Battle = btl_character_turn_update:get_battle(S2Update),

   {TargetCharacter, S3Battle} =
      btl_battle:get_resolved_character(TargetCharacterIX, S2Battle),

   S3Update = btl_character_turn_update:set_battle(S3Battle, S2Update),

   {{_TriggerName, _ReadOnlyContext, S2VolatileContext}, S4Update} =
      apply_conditions
      (
         {OtherTriggerName, ReadOnlyContext, S1VolatileContext},
         TargetCharacter,
         S3Update
      ),

   {S2VolatileContext, S4Update};
apply_mirror_conditions
(
   true,
   OwnTriggerName,
   OtherTriggerName,
   Action,
   {ReadOnlyContext, S0VolatileContext},
   S0Update
) ->
   TargetCharacterIX = btl_action:get_target_index(Action),
   S0Battle = btl_character_turn_update:get_battle(S0Update),

   {TargetCharacter, S1Battle} =
      btl_battle:get_resolved_character(TargetCharacterIX, S0Battle),

   S1Update = btl_character_turn_update:set_battle(S1Battle, S0Update),

   {{_TriggerName, _ReadOnlyContext, S1VolatileContext}, S2Update} =
      apply_conditions
      (
         {OwnTriggerName, ReadOnlyContext, S0VolatileContext},
         TargetCharacter,
         S1Update
      ),

   CharacterIX = btl_action:get_actor_index(Action),
   S2Battle = btl_character_turn_update:get_battle(S2Update),
   {Character, S3Battle} =
      btl_battle:get_resolved_character(CharacterIX, S2Battle),

   S3Update = btl_character_turn_update:set_battle(S3Battle, S2Update),

   {{_TriggerName, _ReadOnlyContext, S2VolatileContext}, S4Update} =
      apply_conditions
      (
         {OtherTriggerName, ReadOnlyContext, S1VolatileContext},
         Character,
         S3Update
      ),

   {S2VolatileContext, S4Update}.

-spec handle_start_of_attack
   (
      list(btl_attack:category()),
      btl_action:type(),
      btl_character_turn_update:type()
   )
   ->
   {list(btl_attack:category()), btl_character_turn_update:type() }.
handle_start_of_attack (S0AttackSequence, Action, S0Update) ->
   S1Update = add_targeting_event(Action, S0Update),

   {S1AttackSequence, S2Update} =
      apply_mirror_conditions
      (
         false,
         ?CONDITION_TRIGGER_START_OF_OWN_ATTACK,
         ?CONDITION_TRIGGER_START_OF_OTHER_ATTACK,
         Action,
         {Action, S0AttackSequence},
         S1Update
      ),

   {S1AttackSequence, S2Update}.

-spec handle_end_of_attack
   (
      btl_action:type(),
      btl_character_turn_update:type()
   )
   -> btl_character_turn_update:type().
handle_end_of_attack (Action, S0Update) ->
   {_None, S1Update} =
      apply_mirror_conditions
      (
         false,
         ?CONDITION_TRIGGER_END_OF_OWN_ATTACK,
         ?CONDITION_TRIGGER_END_OF_OTHER_ATTACK,
         Action,
         {Action, none},
         S0Update
      ),

   S1Update.

-spec commit_hit
   (
      btl_character:type(),
      integer(),
      btl_character:type(),
      integer(),
      list(btl_attack:category()),
      btl_attack:category(),
      btl_action:type(),
      boolean(),
      btl_battle:precision(),
      boolean(),
      btl_character_turn_update:type()
   )
   -> {list(btl_attack:category()), btl_character_turn_update:type()}.
commit_hit
(
   IsParry,
   Precision,
   IsCritical,
   ModdedActor,
   ActorLuck,
   ModdedTarget,
   TargetLuck,
   S0Sequence,
   AttackCategory,
   Action,
   S0Update
) ->
   {ActorIX, TargetIX} =
      case
         (
            ((AttackCategory == counter) and (IsParry == false))
            or ((AttackCategory =/= counter) and (IsParry == true))
         )
      of
         true ->
            {
               btl_action:get_target_index(Action),
               btl_action:get_actor_index(Action)
            };

         false ->
            {
               btl_action:get_actor_index(Action),
               btl_action:get_target_index(Action)
            }
      end,

   S0DamageMultiplier =
      case Precision of
         misses -> 0.0;
         grazes -> 0.5;
         hits -> 1.0
      end,

   S1DamageMultiplier =
      case IsCritical of
         true -> (S0DamageMultiplier) * 2.0;
         false -> 0.0
      end,

   S0AttackDamage =
      shr_omnimods:get_attack_damage
      (
         S1DamageMultiplier,
         shr_character:get_omnimods
         (
            btl_character:get_base_character(ModdedActor)
         ),
         shr_character:get_omnimods
         (
            btl_character:get_base_character(ModdedTarget)
         )
      ),

   {
      {S1AttackDamage, S1Sequence},
      S1Update
   } =
      apply_mirror_conditions
      (
         IsParry,
         ?CONDITION_TRIGGER_COMPUTED_OWN_ATTACK_DAMAGE,
         ?CONDITION_TRIGGER_COMPUTER_OTHER_ATTACK_DAMAGE,
         Action,
         {
            {
               Action,
               AttackCategory,
               IsParry,
               Precision,
               IsCritical,
               ModdedActor,
               ModdedTarget
            },
            {S0AttackDamage, S0Sequence}
         },
         S0Update
      ),

   S0Battle = btl_character_turn_update:get_battle(S1Update),
   {S0Target, S1Battle} = btl_battle:get_resolved_character(TargetIX, S0Battle),
   {S1Target, TargetAtaxiaUpdate} =
      btl_character:ataxia_set_current_health
      (
         (btl_character:get_current_health(S0Target) - S1AttackDamage),
         S0Target
      ),

   {S1Battle, BattleAtaxiaUpdate} =
      btl_battle:set_character
      (
         TargetIX,
         S1Target,
         TargetAtaxiaUpdate,
         S0Battle
      ),

   S2Update =
      btl_character_turn_update:ataxia_set_battle
      (
         S1Battle,
         BattleAtaxiaUpdate,
         S1Update
      ),

   S3Update =
      commit_luck_change
      (
         btl_character:get_player_index(Actor),
         ActorLuck,
         S2Update
      ),

   S4Update =
      commit_luck_change
      (
         btl_character:get_player_index(Target),
         TargetLuck,
         S3Update
      ),

   S5Update =
      btl_character_turn_update:add_to_timeline
      (
         btl_turn_result:new_character_hit
         (
            ActorIX,
            TargetIX,
            AttackCategory,
            Precision,
            IsCritical,
            IsParry,
            S1AttackDamage,
            ActorLuck,
            TargetLuck
         ),
         S4Update
      ),

   {
      S2Sequence,
      S6Update
   } =
      apply_mirror_conditions
      (
         IsParry,
         ?CONDITION_TRIGGER_END_OF_OWN_HIT,
         ?CONDITION_TRIGGER_END_OF_OTHER_HIT,
         Action,
         {
            {
               Action,
               AttackCategory,
               IsParry,
               Precision,
               IsCritical,
               S1AttackDamage,
               ModdedActor,
               ModdedTarget
            },
            S1Sequence
         },
         S5Update
      ),

   {S2Sequence, S6Update}.

-spec handle_critical_hits
   (
      btl_character:type(),
      integer(),
      btl_character:type(),
      integer(),
      list(btl_attack:category()),
      btl_attack:category(),
      btl_action:type(),
      boolean(),
      btl_battle:precision(),
      btl_character_turn_update:type()
   )
   ->
   {
      boolean(),
      btl_character:type(),
      integer(),
      btl_character:type(),
      integer(),
      list(btl_attack:category()),
      btl_character_turn_update:type()
   }.
handle_critical_hits
(
   S0ModdedActor,
   S0ActorLuck,
   S0ModdedTarget,
   S0TargetLuck,
   S0Sequence,
   AttackCategory,
   Action,
   IsParry,
   Precision,
   S0Update
) ->
   {S0IsCritical, S1ActorLuck, S1TargetLuck} =
      roll_for_critical_hits
      (
         S0ModdedActor,
         S0ActorLuck,
         S0ModdedTarget,
         S0TargetLuck
      ),

   {
      {S1IsCritical, S1ModdedActor, S1ModdedTarget, S1Sequence},
      S1Update
   } =
      apply_mirror_conditions
      (
         IsParry,
         ?CONDITION_TRIGGER_ROLLED_FOR_OWN_PRECISION,
         ?CONDITION_TRIGGER_ROLLED_FOR_OTHER_PRECISION,
         Action,
         {
            {Action, AttackCategory, IsParry, Precision},
            {S0IsCritical, S0ModdedActor, S0ModdedTarget, S0Sequence}
         },
         S0Update
      ),

   {
      S1IsCritical,
      S1ModdedActor,
      S1ActorLuck,
      S1ModdedTarget,
      S1TargetLuck,
      S1Sequence,
      S1Update
   }.

-spec handle_precision
   (
      btl_character:type(),
      integer(),
      btl_character:type(),
      integer(),
      list(btl_attack:category()),
      btl_attack:category(),
      btl_action:type(),
      boolean(),
      boolean(),
      btl_character_turn_update:type()
   )
   ->
   {
      btl_attack:precision(),
      btl_character:type(),
      integer(),
      btl_character:type(),
      integer(),
      list(btl_attack:category()),
      btl_character_turn_update:type()
   }.
handle_precision
(
   ModdedActor,
   ActorLuck,
   ModdedTarget,
   TargetLuck,
   Sequence,
   _AttackCategory,
   _Action,
   _IsParry,
   false,
   Update
) ->
   {
      misses,
      ModdedActor,
      ActorLuck,
      ModdedTarget,
      TargetLuck,
      Sequence,
      Update
   };
handle_precision
(
   S0ModdedActor,
   S0ActorLuck,
   S0ModdedTarget,
   S0TargetLuck,
   S0Sequence,
   AttackCategory,
   Action,
   IsParry,
   true,
   S0Update
) ->
   {S0Precision, S1ActorLuck, S1TargetLuck} =
      roll_for_precision
      (
         S0ModdedActor,
         S0ActorLuck,
         S0ModdedTarget,
         S0TargetLuck
      ),

   {
      {S1Precision, S1ModdedActor, S1ModdedTarget, S1Sequence},
      S1Update
   } =
      apply_mirror_conditions
      (
         IsParry,
         ?CONDITION_TRIGGER_ROLLED_FOR_OWN_PRECISION,
         ?CONDITION_TRIGGER_ROLLED_FOR_OTHER_PRECISION,
         Action,
         {
            {Action, AttackCategory, IsParry},
            {S0Precision, S0ModdedActor, S0ModdedTarget, S0Sequence}
         },
         S0Update
      ),

   {
      S1Precision,
      S1ModdedActor,
      S1ActorLuck,
      S1ModdedTarget,
      S1TargetLuck,
      S1Sequence,
      S1Update
   }.

-spec handle_parry
   (
      btl_character:type(),
      integer(),
      btl_character:type(),
      integer(),
      list(btl_attack:category()),
      btl_attack:category(),
      btl_action:type(),
      boolean(),
      btl_character_turn_update:type()
   )
   ->
   {
      boolean(),
      btl_character:type(),
      integer(),
      btl_character:type(),
      integer(),
      list(btl_attack:category()),
      btl_character_turn_update:type()
   }.
handle_parry
(
   ModdedActor,
   ActorLuck,
   ModdedTarget,
   TargetLuck,
   Sequence,
   _AttackCategory,
   _Action,
   true,
   Update
) ->
   {
      false,
      ModdedActor,
      ActorLuck,
      ModdedTarget,
      TargetLuck,
      Sequence,
      Update
   };
handle_parry
(
   S0ModdedActor,
   S0ActorLuck,
   S0ModdedTarget,
   S0TargetLuck,
   S0Sequence,
   AttackCategory,
   Action,
   false,
   S0Update
) ->
   {S0IsParry, S1ActorLuck, S1TargetLuck} =
      roll_for_parry
      (
         S0ModdedActor,
         S0ActorLuck,
         S0ModdedTarget,
         S0TargetLuck
      ),
   {
      {S1IsParry, S1ModdedActor, S1ModdedTarget, S1Sequence},
      S1Update
   } =
      apply_mirror_conditions
      (
         false,
         ?CONDITION_TRIGGER_ROLLED_FOR_OTHER_PARRY,
         ?CONDITION_TRIGGER_ROLLED_FOR_OWN_PARRY,
         Action,
         {
            {Action, AttackCategory},
            {S0IsParry, S0ModdedActor, S0ModdedTarget, S0Sequence}
         },
         S0Update
      ),

   case S1IsParry of
      true ->
         {
            S1IsParry,
            S1ModdedTarget,
            S1TargetLuck,
            S1ModdedActor,
            S1ActorLuck,
            S1Sequence,
            S1Update
         };

      false ->
         {
            S1IsParry,
            S1ModdedActor,
            S1ActorLuck,
            S1ModdedTarget,
            S1TargetLuck,
            S1Sequence,
            S1Update
         }
   end.

-spec handle_double_hits
   (
      btl_character:type(),
      integer(),
      btl_character:type(),
      integer(),
      list(btl_attack:category()),
      btl_attack:category(),
      btl_action:type(),
      btl_character_turn_update:type()
   )
   ->
   {
      boolean(),
      btl_character:type(),
      integer(),
      btl_character:type(),
      integer(),
      list(btl_attack:category()),
      btl_character_turn_update:type()
   }.
handle_double_hits
(
   S0ModdedActor,
   S0ActorLuck,
   S0ModdedTarget,
   S0TargetLuck,
   S0Sequence,
   second,
   Action,
   S0Update
) ->
   {S0CanPerform, S1ActorLuck, S1TargetLuck} =
      roll_for_double_hits
      (
         S0ModdedActor,
         S0ActorLuck,
         S0ModdedTarget,
         S0TargetLuck
      ),

   {
      {S1CanPerform, S1ModdedActor, S1ModdedTarget, S1Sequence},
      S1Update
   } =
      apply_mirror_conditions
      (
         false,
         ?CONDITION_TRIGGER_ROLLED_FOR_OWN_DOUBLE_HITS,
         ?CONDITION_TRIGGER_ROLLED_FOR_OTHER_DOUBLE_HITS,
         Action,
         {
            Action,
            {S0CanPerform, S0ModdedActor, S0ModdedTarget, S0Sequence}
         },
         S0Update
      ),

   {
      S1CanPerform,
      S1ModdedActor,
      S1ActorLuck,
      S1ModdedTarget,
      S1TargetLuck,
      S1Sequence,
      S1Update
   };
handle_double_hits
(
   S0ModdedActor,
   S0ActorLuck,
   S0ModdedTarget,
   S0TargetLuck,
   S0Sequence,
   _,
   Action,
   S0Update
) ->
   {
      true,
      S0ModdedActor,
      S0ActorLuck,
      S0ModdedTarget,
      S0TargetLuck,
      S0Sequence,
      S0Update
   }.

-spec handle_hit
   (
      btl_attack:category(),
      list(btl_attack:category()),
      btl_action:type(),
      btl_character_turn_update:type()
   )
   -> {list(btl_attack:category()), btl_character_turn_update:type()}.
handle_hit (AttackCategory, S0Sequence, Action, S0Update) ->
   {BaseActor, S0ActorLuck, BaseTarget, S0TargetLuck, S1Update} =
      get_actors_and_lucks(AttackCategory, S0Action, S0Update),

   {{S0ModdedActor, S0ModdedTarget, S1Sequence}, S2Update} =
      apply_mirror_conditions
      (
         false,
         ?CONDITION_TRIGGER_DEFINED_ACTORS_FOR_OWN_HIT,
         ?CONDITION_TRIGGER_DEFINED_ACTORS_FOR_OTHER_HIT,
         Action,
         {{Action, AttackCategory}, {BaseActor, BaseTarget, S0Sequence}},
         S1Update
      ),

   case can_perform_hit(S0ModdedActor, S0ModdedTarget) of
      {true, true} ->
         {
            CanPerform,
            S1ModdedActor,
            S1ActorLuck,
            S1ModdedTarget,
            S1TargetLuck,
            S2Sequence,
            S3Update
         } =
            handle_double_hits
            (
               S0ModdedActor,
               S0ActorLuck,
               S0ModdedTarget,
               S0TargetLuck,
               S1Sequence,
               AttackCategory,
               Action,
               S2Update
            ),

         case CanPerform of
            false ->
               S4Update =
                  commit_luck_change
                  (
                     btl_character:get_player_index(S1ModdedActor),
                     S1ActorLuck,
                     S3Update
                  ),

               S5Update =
                  commit_luck_change
                  (
                     btl_character:get_player_index(S1ModdedTarget),
                     S1TargetLuck,
                     S4Update
                  ),

               {S1Sequence, S5Update};

            true ->
               {
                  IsParry,
                  S2ModdedActor,
                  S2ActorLuck,
                  S2ModdedTarget,
                  S2TargetLuck,
                  S3Sequence,
                  S4Update
               } =
                  handle_parry
                  (
                     S1ModdedActor,
                     S1ActorLuck,
                     S1ModdedTarget,
                     S1TargetLuck,
                     S2Sequence,
                     AttackCategory,
                     Action,
                     btl_action:get_is_opportunistic(Action),
                     S3Update
                  ),

               % If 'IsParry' is true, then Actor and Target have been swapped.
               {ActorHasRange, _} =
                  can_perform_hit(S2ModdedActor, S2ModdedTarget),

               {
                  Precision,
                  S2ModdedActor,
                  S2ActorLuck,
                  S2ModdedTarget,
                  S2TargetLuck,
                  S3Sequence,
                  S4Update
               } =
                  handle_precision
                  (
                     S1ModdedActor,
                     S1ActorLuck,
                     S1ModdedTarget,
                     S1TargetLuck,
                     S2Sequence,
                     AttackCategory,
                     Action,
                     IsParry,
                     ActorHasRange,
                     S3Update
                  ),

               {
                  IsCritical,
                  S3ModdedActor,
                  S3ActorLuck,
                  S3ModdedTarget,
                  S3TargetLuck,
                  S4Sequence,
                  S5Update
               } =
                  handle_critical_hit
                  (
                     S2ModdedActor,
                     S2ActorLuck,
                     S2ModdedTarget,
                     S2TargetLuck,
                     S3Sequence,
                     AttackCategory,
                     Action,
                     IsParry,
                     S4Update
                  ),

               {S5Sequence, S6Update} =
                  commit_hit
                  (
                     IsParry,
                     Precision,
                     IsCritical,
                     S3ModdedActor,
                     S3ActorLuck,
                     S3ModdedTarget,
                     S3TargetLuck,
                     S4Sequence,
                     AttackCategory,
                     Action,
                     S5Update
                  ),

               {S5Sequence, S6Update}
         end
      {_, _} -> {S1Sequence, S2Update};
   end.

-spec handle_attack_sequence
   (
      list(btl_attack:category()),
      btl_action:type(),
      btl_character_turn_update:type()
   )
   -> btl_character_turn_update:type().
handle_attack_sequence ([], Action, Update) ->
   {Action, Update};
handle_attack_sequence ([AttackCategory|S0NextElements], Action, S0Update) ->
   {S1NextElements, S1Update} =
      handle_hit(AttackCategory, S0NextElements, Action, S0Update),
   handle_attack_sequence(S1NextElements, Action, S1Update).

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
   S0Sequence = [first, counter, second],

   {S1Sequence, S1Update} =
      handle_start_of_attack(S0Sequence, Action, S0Update),

   S2Update = handle_attack_sequence(S1Sequence, Action, S1Update),
   S3Update = handle_end_of_attack(S2Action, S2Update),

   S3Update.
