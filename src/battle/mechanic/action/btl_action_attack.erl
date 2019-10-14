-module(btl_action_attack).
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
-spec should_reverse_roles (boolean(), btl_attack:category()) -> boolean().
should_reverse_roles (IsParry, AttackCategory) ->
   (
      ((AttackCategory == counter) and (IsParry == false))
      or ((AttackCategory =/= counter) and (IsParry == true))
   ).

-spec apply_condition_to_character
   (
      non_neg_integer(),
      shr_condition:trigger(),
      any(),
      VolatileDataType,
      btl_character_turn_update:type()
   )
   -> {VolatileDataType, btl_character_turn_update:type()}.
apply_condition_to_character
(
   ActorIX,
   Trigger,
   ReadOnlyData,
   S0VolatileData,
   S0Update
) ->
   S0Battle = btl_character_turn_update:get_battle(S0Update),
   {S0Actor, S1Battle} = btl_battle:get_resolved_character(ActorIX, S0Battle),
   S1Update = btl_character_turn_update:set_battle(S1Battle, S0Update),

   {
      S1VolatileContext,
      ActorConditionsAtaxicUpdate,
      S2Update
   } =
      btl_condition:ataxia_apply_trigger
      (
         {Trigger, ReadOnlyData, S0VolatileData},
         S1Update,
         btl_character:get_conditions(S0Actor)
      ),

   %%%%% Actor and Battle may have been modified %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

   S1Battle = btl_character_turn_update:get_battle(S2Update),
   {S1Actor, S2Battle} = btl_battle:get_resolved_character(ActorIX, S1Battle),
   S0Conditions = btl_character:get_conditions(S1Actor),

   S1Conditions =
      ataxic:basic_apply_to(ActorConditionsAtaxicUpdate, S0Conditions),

   {S2Actor, ActorAtaxicUpdate} =
      btl_character:ataxia_set_conditions
      (
         S1Conditions,
         ActorConditionsAtaxicUpdate,
         S1Actor
      ),

   {S3Battle, BattleAtaxicUpdate} =
      btl_battle:ataxia_set_character
      (
         ActorIX,
         S2Actor,
         ActorAtaxicUpdate,
         S2Battle
      ),

   S2Update =
      btl_character_turn_update:ataxia_set_battle
      (
         S3Battle,
         BattleAtaxicUpdate,
         S1Update
      ),

   {S1VolatileContext, S2Update}.

-spec apply_mirror_conditions
   (
      boolean(),
      shr_condition:trigger(),
      shr_condition:trigger(),
      shr_condition:trigger(),
      btl_action:type(),
      {any(), VolatileDataType},
      btl_character_turn_update:type()
   )
   -> {VolatileDataType, btl_character_turn_update:type()}.
apply_mirror_conditions
(
   ReverseRoles,
   OwnTriggerName,
   OtherTriggerName,
   GlobalTriggerName,
   Action,
   {ReadOnlyContext, S0VolatileContext},
   S0Update
) ->
   {ActorIX, TargetIX} =
      case ReverseRoles of
         false ->
            {
               btl_action:get_actor_index(Action),
               btl_action:get_target_index(Action)
            };
         true ->
            {
               btl_action:get_target_index(Action),
               btl_action:get_actor_index(Action)
            }
      end,

   {S1VolatileContext, S1Update} =
      apply_condition_to_character
      (
         ActorIX,
         OwnTriggerName,
         ReadOnlyContext,
         S0VolatileContext,
         S0Update
      ),

   {S2VolatileContext, S2Update} =
      apply_condition_to_character
      (
         TargetIX,
         OtherTriggerName,
         ReadOnlyContext,
         S1VolatileContext,
         S1Update
      ),

   S0Battle = btl_character_turn_update:get_battle(S2Update),

   {
      S3VolatileContext,
      BattleConditionsAtaxicUpdate,
      S5Update
   } =
      btl_condition:ataxia_apply_trigger
      (
         {GlobalTriggerName, ReadOnlyContext, S2VolatileContext},
         S2Update,
         btl_battle:get_conditions(S0Battle)
      ),

   %%%% Battle may have been modified (and very likely has) %%%%%%%%%%%%%%%%%%%%
   S1Battle = btl_character_turn_update:get_battle(S2Update),
   UpdatedBattleConditions =
      ataxic:basic_apply_to
      (
         btl_battle:get_conditions(S1Battle),
         BattleConditionsAtaxicUpdate
      ),

   {S2Battle, BattleAtaxicUpdate} =
      btl_battle:ataxia_set_conditions(UpdatedBattleConditions, S1Battle),

   S5Update =
      btl_character_turn_update:ataxia_set_battle
      (
         S2Battle,
         BattleAtaxicUpdate,
         S2Update
      ),

   {S3VolatileContext, S2Update}.

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

-spec roll_for_double_hit
   (
      btl_character:type(),
      integer(),
      btl_character:type(),
      integer()
   )
   -> {boolean(), integer(), integer()}.
roll_for_double_hit (Actor, ActorLuck, _Target, TargetLuck) ->
   ActorDoubleHitChance =
      shr_attributes:get_double_hit_chance
      (
         shr_character:get_attributes
         (
            btl_character:get_base_character(Actor)
         )
      ),

   {_Roll, IsSuccess, PositiveModifier, NegativeModifier} =
      shr_roll:percentage_with_luck(ActorDoubleHitChance, ActorLuck),

   {
      IsSuccess,
      (ActorLuck + PositiveModifier), % Positive effects are for Actor
      (TargetLuck + NegativeModifier) % Negative effects are for Target
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


-spec handle_start_of_attack
   (
      list(btl_attack:category()),
      btl_action:type(),
      btl_character_turn_update:type()
   )
   ->
   {list(btl_attack:category()), btl_character_turn_update:type() }.
handle_start_of_attack (S0AttackSequence, Action, S0Update) ->
   S1Update =
      btl_character_turn_update:add_to_timeline
      (
         btl_turn_result:new_targeting
         (
            btl_action:get_actor_index(Action),
            btl_action:get_target_index(Action)
         ),
         S0Update
      ),

   {S1AttackSequence, S2Update} =
      apply_mirror_conditions
      (
         false,
         ?CONDITION_TRIGGER_START_OF_OWN_ATTACK,
         ?CONDITION_TRIGGER_START_OF_OTHER_ATTACK,
         ?CONDITION_TRIGGER_START_OF_ANY_ATTACK,
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
         ?CONDITION_TRIGGER_END_OF_ANY_ATTACK,
         Action,
         none,
         S0Update
      ),

   S0Battle = btl_character_turn_update:get_battle(S0Update),
   ActorIX =  btl_action:get_actor_index(Action),
   {S0Actor, S1Battle} = btl_battle:get_resolved_character(ActorIX, S0Battle),
   TargetIX =  btl_action:get_actor_index(Action),
   {S0Target, S2Battle} = btl_battle:get_resolved_character(TargetIX, S1Battle),

   S2Update = btl_character_turn_update:set_battle(S2Battle, S1Update),

   S0ActorIsDead = (not btl_character:get_is_alive(Actor)),
   S0TargetIsDead = (not btl_character:get_is_alive(Actor)),

   S3Update =
      case S0ActorIsDead of
         false -> S2Update;
         true ->
            {_None, NextUpdate} =
               apply_mirror_conditions
               (
                  false,
                  ?CONDITION_TRIGGER_COMPUTED_WAS_KILLED,
                  ?CONDITION_TRIGGER_COMPUTED_HAS_KILLED,
                  ?CONDITION_TRIGGER_COMPUTED_ANY_KILL,
                  Action,
                  none,
                  S2Update
               ),

            NextUpdate
      end,

   S4Update =
      case S0TargetIsDead of
         false -> S3Update;
         true ->
            {_None, NextUpdate} =
               apply_mirror_conditions
               (
                  true,
                  ?CONDITION_TRIGGER_COMPUTED_WAS_KILLED,
                  ?CONDITION_TRIGGER_COMPUTED_HAS_KILLED,
                  ?CONDITION_TRIGGER_COMPUTED_ANY_KILL,
                  Action,
                  none,
                  S3Update
               ),

            NextUpdate
      end,

   S3Battle = btl_character_turn_update:get_battle(S4Update),
   {S1Actor, S1Battle} = btl_battle:get_resolved_character(ActorIX, S0Battle),
   {S1Target, S2Battle} = btl_battle:get_resolved_character(TargetIX, S1Battle),

   S5Update = btl_character_turn_update:set_battle(S2Battle, S4Update),

   S1ActorIsDead = (not btl_character:get_is_alive(Actor)),
   S1TargetIsDead = (not btl_character:get_is_alive(Actor)),

   S6Update =
      case S1ActorIsDead of
         false -> S5Update;
         true ->
            btl_victory_progression:handle_character_loss(S1Actor, S5Update)
      end,

   S7Update =
      case S1TargetIsDead of
         false -> S6Update;
         true ->
            btl_victory_progression:handle_character_loss(S1Target, S6Update)
      end,

   S7Update.

-spec commit_luck_change
   (
      btl_character:type(),
      integer(),
      btl_character_turn_update:type()
   )
   -> btl_character_turn_update:type().
commit_luck_change (Character, NewLuck, S0Update) ->
   PlayerIX = btl_character:get_player_index(Character),

   S0Battle = btl_character_turn_update:get_battle(S0Update),
   S0Player = btl_battle:get_player(PlayerIX, S0Battle),
   {S1Player, PlayerAtaxicUpdate} =
      btl_player:ataxia_set_luck(NewLuck, S0Player),

   {S1Battle, BattleAtaxicUpdate} =
      btl_battle:set_player
      (
         PlayerIX,
         S1Player,
         PlayerAtaxicUpdate,
         S0Battle
      ),

   S1Update =
      btl_character_turn_update:set_battle
      (
         S1Battle,
         BattleAtaxicUpdate,
         S0Update
      ),

   S1Update.

-spec get_character_luck
   (
      btl_character:type(),
      btl_character_turn_update:type()
   )
   -> integer().
get_character_luck (Character, Update) ->
   btl_player:get_luck
   (
      btl_battle:get_player
      (
         btl_character:get_player_index(Character),
         btl_character_turn_update:get_battle(Update)
      )
   ).

-spec get_actors_from_index
   (
      non_neg_integer(),
      non_neg_integer(),
      btl_character_turn_update:type()
   )
   ->
   {
      btl_character:type(),
      btl_character:type(),
      btl_character_turn_update:type()
   }.
get_actors_from_index (C0IX, C1IX, S0Update) ->
   S0Battle = btl_character_turn_update:get_battle(S0Update),
   {C0, S1Battle} = btl_battle:get_resolved_character(C0IX, S0Battle),
   {C1, S2Battle} = btl_battle:get_resolved_character(C1IX, S1Battle),
   S1Update = btl_character_turn_update:set_battle(S2Battle, S0Update),

   {C0, C1, S1Update}.

-spec get_actors
   (
      btl_attack:category(),
      btl_action:type(),
      btl_character_turn_update:type()
   )
   ->
   {
      btl_character:type(),
      btl_character:type(),
      btl_character_turn_update:type()
   }.
get_actors (counter, Action, Update) ->
   get_actors_from_index
   (
      btl_action:get_target_index(Action),
      btl_action:get_actor_index(Action),
      Update
   );
get_actors (_Category, Action, Update) ->
   get_actors_from_index
   (
      btl_action:get_actor_index(Action),
      btl_action:get_target_index(Action),
      Update
   ).

-spec commit_hit
   (
      boolean(),
      btl_battle:precision(),
      boolean(),
      btl_character:type(),
      btl_character:type(),
      list(btl_attack:category()),
      btl_attack:category(),
      btl_action:type(),
      btl_character_turn_update:type()
   )
   -> {list(btl_attack:category()), btl_character_turn_update:type()}.
commit_hit
(
   IsParry,
   Precision,
   IsCritical,
   ModdedActor,
   ModdedTarget,
   S0Sequence,
   AttackCategory,
   Action,
   S0Update
) ->
   {ActorIX, TargetIX} =
      case should_reverse_roles(IsParry, AttackCategory) of
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
         should_reverse_roles(IsParry, AttackCategory),
         ?CONDITION_TRIGGER_COMPUTED_OWN_ATTACK_DAMAGE,
         ?CONDITION_TRIGGER_COMPUTED_OTHER_ATTACK_DAMAGE,
         ?CONDITION_TRIGGER_COMPUTED_ANY_ATTACK_DAMAGE,
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
   {S1Target, TargetAtaxicUpdate} =
      btl_character:ataxia_set_current_health
      (
         (btl_character:get_current_health(S0Target) - S1AttackDamage),
         S0Target
      ),

   {S1Battle, BattleAtaxicUpdate1} =
      btl_battle:set_character
      (
         TargetIX,
         S1Target,
         TargetAtaxicUpdate,
         S0Battle
      ),

   S2Update =
      btl_character_turn_update:ataxia_set_battle
      (
         S1Battle,
         BattleAtaxicUpdate1,
         S1Update
      ),

   S3Update =
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
            get_character_luck(ModdedActor, S2Update),
            get_character_luck(ModdedTarget, S2Update)
         ),
         S2Update
      ),

   {
      S2Sequence,
      S4Update
   } =
      apply_mirror_conditions
      (
         should_reverse_roles(IsParry, AttackCategory),
         ?CONDITION_TRIGGER_END_OF_OWN_HIT,
         ?CONDITION_TRIGGER_END_OF_OTHER_HIT,
         ?CONDITION_TRIGGER_END_OF_ANY_HIT,
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
         S3Update
      ),

   {S2Sequence, S4Update}.

-spec handle_critical_hit
   (
      btl_character:type(),
      btl_character:type(),
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
      btl_character:type(),
      list(btl_attack:category()),
      btl_character_turn_update:type()
   }.
handle_critical_hit
(
   S0ModdedActor,
   S0ModdedTarget,
   S0Sequence,
   AttackCategory,
   Action,
   IsParry,
   Precision,
   S0Update
) ->
   S0ActorLuck = get_character_luck(S0ModdedActor, S0Update),
   S0TargetLuck = get_character_luck(S0ModdedTarget, S0Update),

   {S0IsCritical, S1ActorLuck, S1TargetLuck} =
      roll_for_critical_hit
      (
         S0ModdedActor,
         S0ActorLuck,
         S0ModdedTarget,
         S0TargetLuck
      ),

   S1Update = commit_luck_change(S0ModdedActor, S1ActorLuck, S0Update),
   S2Update = commit_luck_change(S0ModdedTarget, S1TargetLuck, S1Update),

   {
      {S1IsCritical, S1ModdedActor, S1ModdedTarget, S1Sequence},
      S3Update
   } =
      apply_mirror_conditions
      (
         should_reverse_roles(IsParry, AttackCategory),
         ?CONDITION_TRIGGER_ROLLED_FOR_OWN_CRITICAL_HITS,
         ?CONDITION_TRIGGER_ROLLED_FOR_OTHER_CRITICAL_HITS,
         ?CONDITION_TRIGGER_ROLLED_FOR_ANY_CRITICAL_HITS,
         Action,
         {
            {Action, AttackCategory, IsParry, Precision},
            {S0IsCritical, S0ModdedActor, S0ModdedTarget, S0Sequence}
         },
         S2Update
      ),

   {
      S1IsCritical,
      S1ModdedActor,
      S1ModdedTarget,
      S1Sequence,
      S3Update
   }.

-spec handle_precision
   (
      btl_character:type(),
      btl_character:type(),
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
      btl_character:type(),
      list(btl_attack:category()),
      btl_character_turn_update:type()
   }.
handle_precision
(
   ModdedActor,
   ModdedTarget,
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
      ModdedTarget,
      Sequence,
      Update
   };
handle_precision
(
   S0ModdedActor,
   S0ModdedTarget,
   S0Sequence,
   AttackCategory,
   Action,
   IsParry,
   true,
   S0Update
) ->
   S0ActorLuck = get_character_luck(S0ModdedActor, S0Update),
   S0TargetLuck = get_character_luck(S0ModdedTarget, S0Update),

   {S0Precision, S1ActorLuck, S1TargetLuck} =
      roll_for_precision
      (
         S0ModdedActor,
         S0ActorLuck,
         S0ModdedTarget,
         S0TargetLuck
      ),

   S1Update = commit_luck_change(S0ModdedActor, S1ActorLuck, S0Update),
   S2Update = commit_luck_change(S0ModdedTarget, S1TargetLuck, S1Update),

   {
      {S1Precision, S1ModdedActor, S1ModdedTarget, S1Sequence},
      S3Update
   } =
      apply_mirror_conditions
      (
         should_reverse_roles(IsParry, AttackCategory),
         ?CONDITION_TRIGGER_ROLLED_FOR_OWN_PRECISION,
         ?CONDITION_TRIGGER_ROLLED_FOR_OTHER_PRECISION,
         ?CONDITION_TRIGGER_ROLLED_FOR_ANY_PRECISION,
         Action,
         {
            {Action, AttackCategory, IsParry},
            {S0Precision, S0ModdedActor, S0ModdedTarget, S0Sequence}
         },
         S2Update
      ),

   {
      S1Precision,
      S1ModdedActor,
      S1ModdedTarget,
      S1Sequence,
      S3Update
   }.

-spec handle_parry
   (
      btl_character:type(),
      btl_character:type(),
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
      btl_character:type(),
      list(btl_attack:category()),
      btl_character_turn_update:type()
   }.
handle_parry
(
   ModdedActor,
   ModdedTarget,
   Sequence,
   _AttackCategory,
   _Action,
   true,
   Update
) ->
   {
      false,
      ModdedActor,
      ModdedTarget,
      Sequence,
      Update
   };
handle_parry
(
   S0ModdedActor,
   S0ModdedTarget,
   S0Sequence,
   AttackCategory,
   Action,
   false,
   S0Update
) ->
   S0ActorLuck = get_character_luck(S0ModdedActor, S0Update),
   S0TargetLuck = get_character_luck(S0ModdedTarget, S0Update),

   {S0IsParry, S1ActorLuck, S1TargetLuck} =
      roll_for_parry
      (
         S0ModdedActor,
         S0ActorLuck,
         S0ModdedTarget,
         S0TargetLuck
      ),

   S1Update = commit_luck_change(S0ModdedActor, S1ActorLuck, S0Update),
   S2Update = commit_luck_change(S0ModdedTarget, S1TargetLuck, S1Update),

   {
      {S1IsParry, S1ModdedActor, S1ModdedTarget, S1Sequence},
      S3Update
   } =
      apply_mirror_conditions
      (
         should_reverse_roles(false, AttackCategory),
         ?CONDITION_TRIGGER_ROLLED_FOR_OTHER_PARRY,
         ?CONDITION_TRIGGER_ROLLED_FOR_OWN_PARRY,
         ?CONDITION_TRIGGER_ROLLED_FOR_ANY_PARRY,
         Action,
         {
            {Action, AttackCategory},
            {S0IsParry, S0ModdedActor, S0ModdedTarget, S0Sequence}
         },
         S2Update
      ),

   case S1IsParry of
      true ->
         {
            S1IsParry,
            S1ModdedTarget,
            S1ModdedActor,
            S1Sequence,
            S3Update
         };

      false ->
         {
            S1IsParry,
            S1ModdedActor,
            S1ModdedTarget,
            S1Sequence,
            S3Update
         }
   end.

-spec handle_double_hits
   (
      btl_character:type(),
      btl_character:type(),
      list(btl_attack:category()),
      btl_attack:category(),
      btl_action:type(),
      btl_character_turn_update:type()
   )
   ->
   {
      boolean(),
      btl_character:type(),
      btl_character:type(),
      list(btl_attack:category()),
      btl_character_turn_update:type()
   }.
handle_double_hits
(
   S0ModdedActor,
   S0ModdedTarget,
   S0Sequence,
   second,
   Action,
   S0Update
) ->
   S0ActorLuck = get_character_luck(S0ModdedActor, S0Update),
   S0TargetLuck = get_character_luck(S0ModdedTarget, S0Update),

   {S0CanPerform, S1ActorLuck, S1TargetLuck} =
      roll_for_double_hit
      (
         S0ModdedActor,
         S0ActorLuck,
         S0ModdedTarget,
         S0TargetLuck
      ),

   S1Update = commit_luck_change(S0ModdedActor, S1ActorLuck, S0Update),
   S2Update = commit_luck_change(S0ModdedTarget, S1TargetLuck, S1Update),

   {
      {S1CanPerform, S1ModdedActor, S1ModdedTarget, S1Sequence},
      S3Update
   } =
      apply_mirror_conditions
      (
         false,
         ?CONDITION_TRIGGER_ROLLED_FOR_OWN_DOUBLE_HITS,
         ?CONDITION_TRIGGER_ROLLED_FOR_OTHER_DOUBLE_HITS,
         ?CONDITION_TRIGGER_ROLLED_FOR_ANY_DOUBLE_HITS,
         Action,
         {
            Action,
            {S0CanPerform, S0ModdedActor, S0ModdedTarget, S0Sequence}
         },
         S2Update
      ),

   {
      S1CanPerform,
      S1ModdedActor,
      S1ModdedTarget,
      S1Sequence,
      S3Update
   };
handle_double_hits
(
   ModdedActor,
   ModdedTarget,
   Sequence,
   _,
   _Action,
   Update
) ->
   {
      true,
      ModdedActor,
      ModdedTarget,
      Sequence,
      Update
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
   {BaseActor, BaseTarget, S1Update} =
      get_actors(AttackCategory, Action, S0Update),

   {{S0ModdedActor, S0ModdedTarget, S1Sequence}, S2Update} =
      apply_mirror_conditions
      (
         should_reverse_roles(false, AttackCategory),
         ?CONDITION_TRIGGER_DEFINED_ACTORS_FOR_OWN_HIT,
         ?CONDITION_TRIGGER_DEFINED_ACTORS_FOR_OTHER_HIT,
         ?CONDITION_TRIGGER_DEFINED_ACTORS_FOR_ANY_HIT,
         Action,
         {{Action, AttackCategory}, {BaseActor, BaseTarget, S0Sequence}},
         S1Update
      ),

   case can_perform_hit(S0ModdedActor, S0ModdedTarget) of
      {true, true} ->
         {
            CanPerform,
            S1ModdedActor,
            S1ModdedTarget,
            S2Sequence,
            S3Update
         } =
            handle_double_hits
            (
               S0ModdedActor,
               S0ModdedTarget,
               S1Sequence,
               AttackCategory,
               Action,
               S2Update
            ),

         case CanPerform of
            false -> {S1Sequence, S3Update};

            true ->
               {
                  IsParry,
                  S2ModdedActor,
                  S2ModdedTarget,
                  S3Sequence,
                  S4Update
               } =
                  handle_parry
                  (
                     S1ModdedActor,
                     S1ModdedTarget,
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
                  S2ModdedTarget,
                  S3Sequence,
                  S4Update
               } =
                  handle_precision
                  (
                     S1ModdedActor,
                     S1ModdedTarget,
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
                  S3ModdedTarget,
                  S4Sequence,
                  S5Update
               } =
                  handle_critical_hit
                  (
                     S2ModdedActor,
                     S2ModdedTarget,
                     S3Sequence,
                     AttackCategory,
                     Action,
                     IsParry,
                     Precision,
                     S4Update
                  ),

               {S5Sequence, S6Update} =
                  commit_hit
                  (
                     IsParry,
                     Precision,
                     IsCritical,
                     S3ModdedActor,
                     S3ModdedTarget,
                     S4Sequence,
                     AttackCategory,
                     Action,
                     S5Update
                  ),

               {S5Sequence, S6Update}
         end;

      {_, _} -> {S1Sequence, S2Update}
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
   S3Update = handle_end_of_attack(Action, S2Update),

   S3Update.
