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

-spec get_character_abilities
   (
      btl_action:type(),
      btl_character:type(),
      btl_character:type()
   )
   -> {boolean(), boolean(), boolean()}.
get_character_abilities (Action, Character, TargetCharacter) ->
   CharacterWeapon =
      shr_character:get_active_weapon
      (
         btl_character:get_base_character(Character)
      ),

   TargetCharacterWeapon =
      shr_character:get_active_weapon
      (
         btl_character:get_base_character(TargetCharacter)
      ),

   DefenseRange = shr_weapon:get_minimum_range(CharacterWeapon),
   AttackRange = shr_weapon:get_maximum_range(CharacterWeapon),
   TargetDefenseRange = shr_weapon:get_minimum_range(TargetCharacterWeapon),
   TargetAttackRange =  shr_weapon:get_maximum_range(TargetCharacterWeapon),

   IsNotOpportunistic = (not btl_action:get_is_opportunistic(Action)),

   RequiredRange =
      shr_location:dist
      (
         btl_character:get_location(Character),
         btl_character:get_location(TargetCharacter)
      ),

   case (AttackRange >= RequiredRange) of
      true -> ok;
      _ -> error({attack, range, AttackRange, RequiredRange})
   end,

   {
      (DefenseRange == 0),
      (
         IsNotOpportunistic
         and (TargetDefenseRange == 0)
         and (TargetAttackRange >= RequiredRange)
      ),
      (
         IsNotOpportunistic
         and (TargetAttackRange >= RequiredRange)
      )
   }.

-spec compute_luck_changes
   (
      integer(),
      integer(),
      boolean(),
      boolean(),
      integer(),
      integer(),
      integer(),
      integer(),
      integer(),
      integer()
   )
   -> {integer(), integer()}.
compute_luck_changes
(
   AttackerLuck,
   DefenderLuck,
   ParryIsSuccessful,
   HitSomething,
   ParryPositiveLuckMod,
   ParryNegativeLuckMod,
   PrecisionPositiveLuckMod,
   PrecisionNegativeLuckMod,
   CriticalPositiveLuckMod,
   CriticalNegativeLuckMod
) ->
   case {ParryIsSuccessful, HitSomething} of
      {true, true} ->
         {
            (
               AttackerLuck
               % Attacker wasn't the one parrying
               + ParryNegativeLuckMod
               % Attacker was the one evading
               + PrecisionPositiveLuckMod
               % miss -> no critical hit luck modifier
            ),
            (
               DefenderLuck
               % Defender was the one parrying
               + ParryPositiveLuckMod
               % Defender wasn't the one evading
               + PrecisionNegativeLuckMod
               % miss -> no critical hit luck modifier
            )
         };

      {true, false} ->
         {
            (
               AttackerLuck
               % Attacker wasn't the one parrying
               + ParryNegativeLuckMod
               % Attacker was the one evading
               + PrecisionPositiveLuckMod
               % Attacker wasn't the one doing a critical
               + CriticalNegativeLuckMod
            ),
            (
               DefenderLuck
               % Defender was the one parrying
               + ParryPositiveLuckMod
               % Defender wasn't the one evading
               + PrecisionNegativeLuckMod
               % Defender was the one doing a critical
               + CriticalPositiveLuckMod
            )
         };

      {false, true} ->
         {
            (
               AttackerLuck
               % Attacker wasn't the one parrying
               + ParryNegativeLuckMod
               % Defender was the one evading
               + PrecisionNegativeLuckMod
               % miss -> no critical hit luck modifier
            ),
            (
               DefenderLuck
               % Defender was the one parrying
               + ParryPositiveLuckMod
               % Defender was the one evading
               + PrecisionPositiveLuckMod
               % miss -> no critical hit luck modifier
            )
         };

      {false, false} ->
         {
            (
               AttackerLuck
               % Attacker wasn't the one parrying
               + ParryNegativeLuckMod
               % Attacker wasn't the one evading
               + PrecisionNegativeLuckMod
               % Attacker was the one doing a critical
               + CriticalPositiveLuckMod
            ),
            (
               DefenderLuck
               % Defender was the one parrying
               + ParryPositiveLuckMod
               % Defender was the one evading
               + PrecisionPositiveLuckMod
               % Defender wasn't the one doing a critical
               + CriticalNegativeLuckMod
            )
         }
   end.

-spec effect_of_attack
   (
      btl_attack:category(),
      btl_character:type(),
      btl_character:type(),
      integer(),
      integer(),
      boolean()
   )
   ->
   {
      btl_character:type(),
      btl_character:type(),
      integer(),
      integer(),
      btl_attack:type()
   }.
effect_of_attack
(
   Category,
   Character,
   TargetCharacter,
   Luck,
   TargetLuck,
   TargetCanParry
) ->
   {ParryIsSuccessful, ParryPositiveLuckMod, ParryNegativeLuckMod} =
      case TargetCanParry of
         true ->
            TargetAttributes =
               shr_character:get_attributes
               (
                  btl_character:get_base_character(TargetCharacter)
               ),
            roll_parry(TargetAttributes, TargetLuck);

         false -> {false, 0, 0}
      end,

   {Attacker, S0Defender, S0AttackerLuck, S0DefenderLuck} =
      case ParryIsSuccessful of
         false -> {Character, TargetCharacter, Luck, TargetLuck};
         true -> {TargetCharacter, Character, TargetLuck, Luck}
      end,

   AttackerBaseCharacter = btl_character:get_base_character(Attacker),
   AttackerAttributes = shr_character:get_attributes(AttackerBaseCharacter),
   DefenderBaseCharacter = btl_character:get_base_character(S0Defender),
   DefenderAttributes = shr_character:get_attributes(DefenderBaseCharacter),

   {PrecisionModifier, PrecisionPositiveLuckMod, PrecisionNegativeLuckMod} =
      roll_precision_modifier
      (
         AttackerAttributes,
         DefenderAttributes,
         S0DefenderLuck
      ),

   {CriticalModifier, CriticalPositiveLuckMod, CriticalNegativeLuckMod} =
      roll_critical_modifier(AttackerAttributes, S0AttackerLuck),

   Damage =
      shr_omnimods:get_attack_damage
      (
         (
            PrecisionModifier
            * CriticalModifier
            * shr_attributes:get_damage_multiplier(AttackerAttributes)
         ),
         shr_character:get_omnimods(AttackerBaseCharacter),
         shr_character:get_omnimods(DefenderBaseCharacter)
      ),

   {S1AttackerLuck, S1DefenderLuck} =
      compute_luck_changes
      (
         S0AttackerLuck,
         S0DefenderLuck,
         ParryIsSuccessful,
         (PrecisionModifier > 0.0),
         ParryPositiveLuckMod,
         ParryNegativeLuckMod,
         PrecisionPositiveLuckMod,
         PrecisionNegativeLuckMod,
         CriticalPositiveLuckMod,
         CriticalNegativeLuckMod
      ),

   AttackReport =
      btl_attack:new
      (
         Category,
         PrecisionModifier,
         CriticalModifier,
         ParryIsSuccessful,
         Damage
      ),

   % If we "ataxia update" here, we'll get redundant ataxia updates, since
   % both luck and health are likely to change again soon.
   % If we don't "ataxia update" here, we'll have a bit of an ugly hack at the
   % end that looks like: ataxia_set_current_health(get_current_health, char).
   % I'm choosing to go with the hack. The function should not return a
   % character_turn_update struct though, to make it clear the ataxia updates
   % are still required.
   S1Defender =
      btl_character:set_current_health
      (
         (btl_character:get_current_health(S0Defender) - Damage),
         S0Defender
      ),

   case ParryIsSuccessful of
      false ->
         {Attacker, S1Defender, S1AttackerLuck, S1DefenderLuck, AttackReport};

      true ->
         {S1Defender, Attacker, S1DefenderLuck, S1AttackerLuck, AttackReport}
   end.

-spec handle_attack_sequence
   (
      list({btl_attack:category(), boolean()}),
      btl_character:type(),
      btl_character:type(),
      integer(),
      integer(),
      list(btl_attack:type())
   )
   ->
   {
      btl_character:type(),
      btl_character:type(),
      integer(),
      integer(),
      list(btl_attack:type())
   }.
handle_attack_sequence
(
   [],
   Character,
   TargetCharacter,
   PlayerLuck,
   TargetPlayerLuck,
   Results
)
->
   {
      Character,
      TargetCharacter,
      PlayerLuck,
      TargetPlayerLuck,
      lists:reverse(Results)
   };
handle_attack_sequence
(
   [{first, TargetCanParry}|NextAttacks],
   S0Character,
   S0TargetCharacter,
   S0PlayerLuck,
   S0TargetPlayerLuck,
   Results
)
->
   case
      (
         (btl_character:get_current_health(S0Character) > 0)
         and (btl_character:get_current_health(S0TargetCharacter) > 0)
      )
   of
      true ->
         {
            S1Character,
            S1TargetCharacter,
            S1PlayerLuck,
            S1TargetPlayerLuck,
            Result
         } =
            effect_of_attack
            (
               first,
               S0Character,
               S0TargetCharacter,
               S0PlayerLuck,
               S0TargetPlayerLuck,
               TargetCanParry
            ),

         handle_attack_sequence
         (
            NextAttacks,
            S1Character,
            S1TargetCharacter,
            S1PlayerLuck,
            S1TargetPlayerLuck,
            [Result|Results]
         );

      false ->
         {
            S0Character,
            S0TargetCharacter,
            S0PlayerLuck,
            S0TargetPlayerLuck,
            lists:reverse(Results)
         }
   end;
handle_attack_sequence
(
   [{counter, CanParry}|NextAttacks],
   S0Character,
   S0TargetCharacter,
   S0PlayerLuck,
   S0TargetPlayerLuck,
   Results
)
->
   case
      (
         (btl_character:get_current_health(S0Character) > 0)
         and (btl_character:get_current_health(S0Character) > 0)
      )
   of
      true ->
         {
            S1TargetCharacter,
            S1Character,
            S1TargetPlayerLuck,
            S1PlayerLuck,
            Result
         } =
            effect_of_attack
            (
               counter,
               S0TargetCharacter,
               S0Character,
               S0TargetPlayerLuck,
               S0PlayerLuck,
               CanParry
            ),

         handle_attack_sequence
         (
            NextAttacks,
            S1Character,
            S1TargetCharacter,
            S1PlayerLuck,
            S1TargetPlayerLuck,
            [Result|Results]
         );

      false ->
         {
            S0Character,
            S0TargetCharacter,
            S0PlayerLuck,
            S0TargetPlayerLuck,
            lists:reverse(Results)
         }
   end;
handle_attack_sequence
(
   [{second, TargetCanParry}|NextAttacks],
   S0Character,
   S0TargetCharacter,
   S0PlayerLuck,
   S0TargetPlayerLuck,
   Results
)
->
   case
      (
         (btl_character:get_current_health(S0Character) > 0)
         and (btl_character:get_current_health(S0TargetCharacter) > 0)
      )
   of
      true ->
         Attributes =
            shr_character:get_attributes
            (
               btl_character:get_base_character(S0Character)
            ),
         DoubleAttackChance = shr_attributes:get_double_hit_chance(Attributes),
         {_Roll, IsSuccessful, PositiveModifier, NegativeModifier} =
            shr_roll:percentage_with_luck(DoubleAttackChance, S0PlayerLuck),

         S1PlayerLuck = (S0PlayerLuck + PositiveModifier),
         S1TargetPlayerLuck = (S0TargetPlayerLuck + NegativeModifier),

         case IsSuccessful of
            false ->
               handle_attack_sequence
               (
                  NextAttacks,
                  S0Character,
                  S0TargetCharacter,
                  S1PlayerLuck,
                  S1TargetPlayerLuck,
                  Results
               );

            true ->
               {
                  S1Character,
                  S1TargetCharacter,
                  S2PlayerLuck,
                  S2TargetPlayerLuck,
                  Result
               } =
                  effect_of_attack
                  (
                     second,
                     S0Character,
                     S0TargetCharacter,
                     S1PlayerLuck,
                     S1TargetPlayerLuck,
                     TargetCanParry
                  ),

               handle_attack_sequence
               (
                  NextAttacks,
                  S1Character,
                  S1TargetCharacter,
                  S2PlayerLuck,
                  S2TargetPlayerLuck,
                  [Result|Results]
               )
         end;

      false ->
         {
            S0Character,
            S0TargetCharacter,
            S0PlayerLuck,
            S0TargetPlayerLuck,
            lists:reverse(Results)
         }
   end.

-spec apply_luck_decay (integer()) -> integer().
apply_luck_decay (Luck) ->
   case {(Luck =< -2), (Luck >= 2)}  of
      {true, _} -> (Luck + 2);
      {_, true} -> (Luck - 2);
      _ -> 0
   end.

-spec apply_mirror_conditions
   (
      shr_condition:trigger(),
      shr_condition:trigger(),
      btl_action:type(),
      {any(), VolatileContext},
      btl_character_turn_update:type()
   )
   -> {VolatileContext, btl_character_turn_update:type()}.
apply_mirror_conditions
(
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
         ?CONDITION_TRIGGER_END_OF_OWN_ATTACK,
         ?CONDITION_TRIGGER_END_OF_OTHER_ATTACK,
         Action,
         {Action, none},
         S0Update
      ),

   S1Update.

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
         ?CONDITION_TRIGGER_ACTORS_DEFINITION_FOR_OWN_HIT,
         ?CONDITION_TRIGGER_ACTORS_DEFINITION_FOR_OTHER_HIT,
         Action,
         {{Action, AttackCategory}, {BaseActor, BaseTarget, S0Sequence}},
         S1Update
      ),

   case can_perform_attack(S0ModdedActor, S0ModdedTarget) of
      false -> {S1Sequence, S2Update};
      true ->
         {S0IsParry, S1ActorLuck, S1TargetLuck} =
            roll_for_parry
            (
               S0ModdedActor,
               S0ActorLuck,
               S0ModdedTarget,
               S0TargetLuck
            ),
         {
            {S1IsParry, S1ModdedActor, S1ModdedTarget, S2Sequence},
            S3Update
         } =
            apply_mirror_conditions
            (
               ?CONDITION_TRIGGER_ACTORS_DEFINITION_FOR_OTHER_PARRY,
               ?CONDITION_TRIGGER_ACTORS_DEFINITION_FOR_OWN_PARRY,
               Action,
               {
                  {Action, AttackCategory},
                  {S0IsParry, S0ModdedActor, S0ModdedTarget, S1Sequence}
               },
               S2Update
            ),

         % TODO
         {S2Sequence, S3Update}
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

   PlayerIX = btl_character:get_player_index(S0Character),
   S0Player = btl_battle:get_player(PlayerIX, S0Battle),
   S0PlayerLuck = btl_player:get_luck(S0Player),

   TargetPlayerIX = btl_character:get_player_index(S0TargetCharacter),
   S0TargetPlayer = btl_battle:get_player(TargetPlayerIX, S1Battle),
   S0TargetPlayerLuck = btl_player:get_luck(S0TargetPlayer),

   {CanParry, TargetCanParry, TargetCanCounter} =
      get_character_abilities(Action, S0Character, S0TargetCharacter),

   {
      S1Character,
      S1TargetCharacter,
      S1PlayerLuck,
      S1TargetPlayerLuck,
      AttackReports
   } =
      handle_attack_sequence
      (
         case TargetCanCounter of
            true ->
               [
                  {first, TargetCanParry},
                  {counter, CanParry},
                  {second, TargetCanParry}
               ];

            false ->
               [
                  {first, TargetCanParry},
                  {second, TargetCanParry}
               ]
         end,
         S0Character,
         S0TargetCharacter,
         S0PlayerLuck,
         S0TargetPlayerLuck,
         []
      ),

   S2PlayerLuck = apply_luck_decay(S1PlayerLuck),
   S2TargetPlayerLuck = apply_luck_decay(S1TargetPlayerLuck),

   CharacterAtaxiaUpdate =
      ataxic:update_field
      (
         btl_character:get_current_health_field(),
         ataxic:constant(btl_character:get_current_health(S1Character))
      ),

   {S2Battle, BattleAtaxiaUpdate0} =
      btl_battle:ataxia_set_character
      (
         CharacterIX,
         S1Character,
         CharacterAtaxiaUpdate,
         S1Battle
      ),

   TargetCharacterAtaxiaUpdate =
      ataxic:update_field
      (
         btl_character:get_current_health_field(),
         ataxic:constant(btl_character:get_current_health(S1TargetCharacter))
      ),

   {S3Battle, BattleAtaxiaUpdate1} =
      btl_battle:ataxia_set_character
      (
         TargetCharacterIX,
         S1TargetCharacter,
         TargetCharacterAtaxiaUpdate,
         S2Battle
      ),

   {S1Player, PlayerAtaxiaUpdate} =
      btl_player:ataxia_set_luck(S2PlayerLuck, S0Player),

   {S4Battle, BattleAtaxiaUpdate2} =
      btl_battle:ataxia_set_player
      (
         PlayerIX,
         S1Player,
         PlayerAtaxiaUpdate,
         S3Battle
      ),

   {S1TargetPlayer, TargetPlayerAtaxiaUpdate} =
      btl_player:ataxia_set_luck(S2TargetPlayerLuck, S0TargetPlayer),

   {S5Battle, BattleAtaxiaUpdate3} =
      btl_battle:ataxia_set_player
      (
         TargetPlayerIX,
         S1TargetPlayer,
         TargetPlayerAtaxiaUpdate,
         S4Battle
      ),

   S1Update =
      btl_character_turn_update:ataxia_set_battle
      (
         S5Battle,
         ataxic:optimize
         (
            ataxic:sequence
            (
               [
                  BattleAtaxiaUpdate0,
                  BattleAtaxiaUpdate1,
                  BattleAtaxiaUpdate2,
                  BattleAtaxiaUpdate3
               ]
            )
         ),
         S0Update
      ),

   S2Update =
      btl_character_turn_update:add_to_timeline
      (
         btl_turn_result:new_character_attacked
         (
            CharacterIX,
            TargetCharacterIX,
            AttackReports,
            S2PlayerLuck,
            S2TargetPlayerLuck
         ),
         S1Update
      ),

   S3Update =
      case (btl_character:get_current_health(S1Character) > 0) of
         true -> S2Update;
         false ->
            % Remove melee opportunistic zone for S1Character.
            btl_victory_progression:handle_character_loss(S1Character, S2Update)
      end,

   S4Update =
      case (btl_character:get_current_health(S1TargetCharacter) > 0) of
         true -> S3Update;
         false ->
            % Remove melee opportunistic zone for S1TargetCharacter.
            btl_victory_progression:handle_character_loss
            (
               S1TargetCharacter,
               S3Update
            )
      end,

   {ok, S4Update}.
