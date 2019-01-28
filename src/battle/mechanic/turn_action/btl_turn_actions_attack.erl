-module(btl_turn_actions_attack).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
-spec handle_attack_sequence
   (
      btl_character_current_data:type(),
      non_neg_integer(),
      btl_character_current_data:type(),
      non_neg_integer(),
      list(btl_attack:step()),
      integer(),
      integer()
   )
   ->
   {
      list(btl_attack:type()),
      non_neg_integer(),
      non_neg_integer(),
      integer(),
      integer()
   }.
handle_attack_sequence
(
   CharacterCurrentData,
   CharacterCurrentHealth,
   TargetCurrentData,
   TargetCurrentHealth,
   AttackSequence,
   AttackerLuck,
   DefenderLuck
) ->
   % TODO lists:foldl over AttackSequence to take luck into account.
   AttackPlannedEffects =
      lists:map
      (
         fun (AttackStep) ->
            btl_attack:get_description_of
            (
               AttackStep,
               CharacterCurrentData,
               TargetCurrentData,
               AttackerLuck,
               DefenderLuck
            )
         end,
         AttackSequence
      ),

   lists:foldl
   (
      fun
      (
         AttackEffectCandidate,
         {AttackValidEffects, AttackerHealth, DefenderHealth}
      ) ->
         {AttackResult, NewAttackerHealth, NewDefenderHealth} =
            btl_attack:apply_to_healths
            (
               AttackEffectCandidate,
               AttackerHealth,
               DefenderHealth
            ),
         case AttackResult of
            nothing -> {AttackValidEffects, AttackerHealth, DefenderHealth};
            _ ->
               {
                  (AttackValidEffects ++ [AttackResult]),
                  NewAttackerHealth,
                  NewDefenderHealth
               }
         end
      end,
      {
         [],
         CharacterCurrentHealth,
         TargetCurrentHealth
      },
      AttackPlannedEffects
   ).

-spec get_attack_sequence
   (
      btl_character:type(),
      btl_character:type()
   )
   -> list(btl_attack:step()).
get_attack_sequence (Character, TargetCharacter) ->
   Range =
      btl_location:dist
      (
         btl_character:get_location(Character),
         btl_character:get_location(TargetCharacter)
      ),

   {AttackingWeaponID, _} = btl_character:get_weapon_ids(Character),
   {DefendingWeaponID, _} = btl_character:get_weapon_ids(TargetCharacter),

   AttackingWeapon = shr_weapon:from_id(AttackingWeaponID),
   DefendingWeapon = shr_weapon:from_id(DefendingWeaponID),

   btl_attack:get_sequence(Range, AttackingWeapon, DefendingWeapon).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec handle
   (
      btl_battle_action:type(),
      btl_character_turn_update:type()
   )
   -> btl_character_turn_update:type().
handle (BattleAction, Update) ->
   Data = btl_character_turn_update:get_data(Update),
   Battle = btl_character_turn_data:get_battle(Data),
   Character = btl_character_turn_data:get_character(Data),
   CharacterIX = btl_character_turn_data:get_character_ix(Data),
   CharacterCurrentData =
      btl_character_turn_data:get_character_current_data(Data),
   AttackingPlayer =
      btl_battle:get_player(btl_character:get_player_ix(Character), Battle),
   AttackingPlayerLuck = btl_player:get_luck(AttackingPlayer),


   Map = btl_battle:get_map(Battle),
   TargetIX = btl_battle_action:get_target_ix(BattleAction),
   TargetCharacter = btl_battle:get_character(TargetIX, Battle),
   TargetCurrentData = btl_character_current_data:new(TargetCharacter, Map),
   DefendingPlayer =
      btl_battle:get_player
      (
         btl_character:get_player_ix(TargetCharacter),
         Battle
      ),
   DefendingPlayerLuck = btl_player:get_luck(DefendingPlayer),

   true = btl_character:get_is_alive(TargetCharacter),

   AttackSequence = get_attack_sequence(Character, TargetCharacter),

   {
      AttackEffects,
      RemainingAttackerHealth,
      RemainingDefenderHealth,
      _NewAttackerLuck,
      _NewDefenderLuck
   } =
      handle_attack_sequence
      (
         CharacterCurrentData,
         btl_character:get_current_health(Character),
         TargetCurrentData,
         btl_character:get_current_health(TargetCharacter),
         AttackSequence,
         AttackingPlayerLuck,
         DefendingPlayerLuck
      ),

   % TODO: update lucks...

   UpdatedCharacter =
      btl_character:set_current_health(RemainingAttackerHealth, Character),

   UpdatedBattle =
      btl_battle:set_character
      (
         TargetIX,
         btl_character:set_current_health
         (
            RemainingDefenderHealth,
            TargetCharacter
         ),
         Battle
      ),

   S0Data = btl_character_turn_data:set_battle(UpdatedBattle, Data),
   S1Data = btl_character_turn_data:set_character(UpdatedCharacter, S0Data),

   TimelineItem =
      btl_turn_result:new_character_attacked
      (
         CharacterIX,
         TargetIX,
         AttackEffects
      ),

   DBQuery0 =
      ataxic:update_field
      (
         btl_battle:get_characters_field(),
         ataxic_sugar:update_orddict_element
         (
            TargetIX,
            ataxic:update_field
            (
               btl_character:get_current_health_field(),
               ataxic:constant(RemainingDefenderHealth)
            )
         )
      ),

   DBQuery1 =
      ataxic:update_field
      (
         btl_battle:get_characters_field(),
         ataxic_sugar:update_orddict_element
         (
            CharacterIX,
            ataxic:update_field
            (
               btl_character:get_current_health_field(),
               ataxic:constant(RemainingAttackerHealth)
            )
         )
      ),

   S0Update =
      btl_character_turn_update:add_to_timeline
      (
         TimelineItem,
         DBQuery0,
         Update
      ),

   S1Update = btl_character_turn_update:add_to_db(DBQuery1, S0Update),
   S2Update = btl_character_turn_update:set_data(S1Data, S1Update),

   S3Update =
      btl_victory:handle_character_lost_health
      (
         CharacterIX,
         RemainingAttackerHealth,
         S2Update
      ),

   S4Update =
      btl_victory:handle_character_lost_health
      (
         TargetIX,
         RemainingDefenderHealth,
         S3Update
      ),

   S4Update.
