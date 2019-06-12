-module(btl_action_move).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-type attack_candidate_ref() ::
   {
      non_neg_integer(), % Character IX
      shr_location:type(), % Character Location
      non_neg_integer() % Character attack range
   }.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export
(
   [
      handle/3
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec generate_attacks_of_opportunity_candidates
   (
      btl_character:either(),
      shr_location:type(),
      orddict:orddict(non_neg_integer(), btl_character:either()),
      non_neg_integer()
   )
   -> list(attack_candidate_ref()).
generate_attacks_of_opportunity_candidates
(
   Character,
   Location,
   Characters,
   StepsCount
) ->
   PlayerIX = btl_character:get_player_index(Character),
   orddict:fold
   (
      fun (CandidateIX, Candidate, Results) ->
         case
            (
               (btl_character:get_player_index(Candidate) == PlayerIX)
               or (not btl_character:get_is_alive(Candidate))
            )
         of
            true -> Results;
            false ->
               CandidateWeapon =
                  shr_character:get_active_weapon
                  (
                     btl_character:get_base_character(Candidate)
                  ),
               case (shr_weapon:get_minimum_range(CandidateWeapon) > 0) of
                  true -> Results;
                  false ->
                     CandidateLocation = btl_character:get_location(Candidate),
                     CandidateWeaponRange =
                        shr_weapon:get_maximum_range(CandidateWeapon),

                     Range =
                        (
                           shr_location:dist(Location, CandidateLocation)
                           - CandidateWeaponRange
                        ),

                     case (Range =< StepsCount) of
                        false -> Results;
                        true ->
                           [
                              {
                                 CandidateIX,
                                 CandidateLocation,
                                 CandidateWeaponRange
                              }
                              |Results
                           ]
                     end
               end
         end
      end,
      [],
      Characters
   ).

-spec detect_attacks_of_opportunity
   (
      shr_location:type(),
      list(attack_candidate_ref()),
      non_neg_integer()
   )
   -> {list(attack_candidate_ref()), list(non_neg_integer())}.
detect_attacks_of_opportunity (Location, Candidates, RemainingStepsCount) ->
   lists:foldl
   (
      fun (Candidate, {FutureCandidates, Attackers}) ->
         {CandidateIX, CandidateLocation, CandidateAttackRange} = Candidate,
         Range =
            (
               shr_location:dist(Location, CandidateLocation)
               - CandidateAttackRange
            ),
         if
            (Range =< 0) -> {FutureCandidates, [CandidateIX|Attackers]};
            (Range =< RemainingStepsCount) ->
               {[Candidate|FutureCandidates], Attackers};
            true -> {FutureCandidates, Attackers}
         end
      end,
      {[], []},
      Candidates
   ).

-spec generate_forbidden_locations
   (
      non_neg_integer(),
      orddict:orddict(non_neg_integer(), btl_character:either())
   )
   -> sets:set(shr_location:type()).
generate_forbidden_locations (CharacterIX, Characters) ->
   orddict:fold
   (
      fun (IX, Char, Prev) ->
         IsAlive = btl_character:get_is_alive(Char),
         if
            (IX == CharacterIX) -> Prev;
            (not IsAlive) -> Prev;
            true ->
               sets:add_element(btl_character:get_location(Char), Prev)
         end
      end,
      sets:new(),
      Characters
   ).

-spec cross
   (
      list(shr_direction:enum()),
      shr_location:type(),
      non_neg_integer(),
      btl_character:either(),
      shr_map:type(),
      sets:set(shr_location:type()),
      list(attack_candidate_ref()),
      non_neg_integer(),
      list(shr_direction:enum()),
      non_neg_integer()
   )
   ->
   {
      shr_location:type(),
      list(shr_direction:type()),
      non_neg_integer(),
      list(btl_action:type()),
      list(shr_direction:type())
   }.
cross
(
   [],
   Location,
   _CharacterIX,
   _Character,
   _Map,
   _ForbiddenLocations,
   _AttacksOfOpportunityCandidates,
   _RemainingStepsCount,
   ReversedHandledPath,
   Cost
) ->
   {Location, [], Cost, [], lists:reverse(ReversedHandledPath)};
cross
(
   [Step|NextSteps],
   Location,
   CharacterIX,
   Character,
   Map,
   ForbiddenLocations,
   AttacksOfOpportunityCandidates,
   RemainingStepsCount,
   ReversedHandledPath,
   Cost
) ->
   NextLocation = shr_location:apply_direction(Step, Location),
   NextTileInstance = shr_map:get_tile_instance(NextLocation, Map),
   NextTileClassID = shr_tile_instance:get_tile_id(NextTileInstance),
   NextTile = shr_tile:from_id(NextTileClassID),
   NextCost = (Cost + shr_tile:get_cost(NextTile)),
   NextRemainingStepsCount = (RemainingStepsCount - 1),
   IsForbidden = sets:is_element(NextLocation, ForbiddenLocations),

   false = IsForbidden,

   {NextAttacksOfOpportunityCandidates, Attackers} =
      detect_attacks_of_opportunity
      (
         NextLocation,
         AttacksOfOpportunityCandidates,
         NextRemainingStepsCount
      ),

   TriggerInterruptions =
      ordsets:fold
      (
         fun (MarkerName, CurrentInterruptions) ->
            case shr_map:get_marker(MarkerName, Map) of
               {ok, Marker} ->
                  case
                     shr_map_marker:interrupts_movement
                     (
                        btl_character:get_player_index(Character),
                        Marker
                     )
                  of
                     true ->
                        [
                           btl_action:from_map_marker
                           (
                              CharacterIX,
                              Character,
                              Marker
                           )
                           |CurrentInterruptions
                        ];

                     _ -> CurrentInterruptions
                  end;

               error -> CurrentInterruptions
            end
         end,
         [],
         shr_tile_instance:get_triggers(NextTileInstance)
      ),

   AttackOfOpportunityInterruptions =
      lists:map
      (
         fun (AttackerIX) ->
            btl_action:new_attack_of_opportunity(AttackerIX, CharacterIX)
         end,
         Attackers
      ),

   Interruptions = (TriggerInterruptions ++ AttackOfOpportunityInterruptions),

   case Interruptions of
      [] ->
         cross
         (
            NextSteps,
            NextLocation,
            CharacterIX,
            Character,
            Map,
            ForbiddenLocations,
            NextAttacksOfOpportunityCandidates,
            NextRemainingStepsCount,
            [Step|ReversedHandledPath],
            NextCost
         );

      _ ->
         {
            NextLocation,
            NextSteps,
            NextCost,
            Interruptions,
            lists:reverse([Step|ReversedHandledPath])
         }
   end.

-spec cross
   (
      list(shr_direction:enum()),
      shr_location:type(),
      non_neg_integer(),
      btl_character:either(),
      shr_map:type(),
      sets:set(shr_location:type()),
      list(attack_candidate_ref()),
      non_neg_integer()
   )
   ->
   {
      shr_location:type(),
      list(shr_direction:type()),
      non_neg_integer(),
      list(btl_action:type()),
      list(shr_direction:type())
   }.
cross
(
   Path,
   Location,
   CharacterIX,
   Character,
   Map,
   ForbiddenLocations,
   AttacksOfOpportunityCandidates,
   RemainingStepsCount
) ->
   cross
   (
      Path,
      Location,
      CharacterIX,
      Character,
      Map,
      ForbiddenLocations,
      AttacksOfOpportunityCandidates,
      RemainingStepsCount,
      [],
      0
   ).

-spec get_path_cost_and_destination
   (
      non_neg_integer(),
      btl_character:type(),
      btl_character_turn_update:type(),
      list(shr_direction:type())
   )
   ->
   {
      shr_location:type(),
      list(shr_direction:type()),
      non_neg_integer(),
      list(btl_action:type()),
      list(shr_direction:type())
   }.
get_path_cost_and_destination (CharacterIX, Character, Update, Path) ->
   Battle = btl_character_turn_update:get_battle(Update),
   Map = btl_battle:get_map(Battle),
   Characters = btl_battle:get_characters(Battle),
   Location = btl_character:get_location(Character),
   PathSteps = length(Path),

   ForbiddenLocations = generate_forbidden_locations(CharacterIX, Characters),
   AttacksOfOpportunityCandidates =
      generate_attacks_of_opportunity_candidates
      (
         Character,
         Location,
         Characters,
         PathSteps
      ),

   cross
   (
      Path,
      Location,
      CharacterIX,
      Character,
      Map,
      ForbiddenLocations,
      AttacksOfOpportunityCandidates,
      PathSteps
   ).

-spec get_movement_points
   (
      btl_action:type(),
      btl_character:type()
   )
   -> non_neg_integer().
get_movement_points (Action, Character) ->
   case btl_action:get_movement_points(Action) of
      -1 ->
         shr_statistics:get_movement_points
         (
            shr_character:get_statistics
            (
               btl_character:get_base_character(Character)
            )
         );

      Other -> Other
   end.

-spec commit_move
   (
      non_neg_integer(),
      btl_character:type(),
      btl_character_turn_update:type(),
      list(shr_direction:type()),
      shr_location:type()
   )
   -> btl_character_turn_update:type().
commit_move (CharacterIX, Character, S0Update, Path, NewLocation) ->
   S0Battle = btl_character_turn_update:get_battle(S0Update),

   Map = btl_battle:get_map(S0Battle),

   TileOmnimods =
      shr_tile:get_omnimods
      (
         shr_tile:from_id
         (
            shr_tile_instance:get_tile_id
            (
               shr_map:get_tile_instance(NewLocation, Map)
            )
         )
      ),

   {UpdatedCharacter, CharacterAtaxiaUpdate} =
      btl_character:ataxia_set_location(NewLocation, TileOmnimods, Character),

   {UpdatedBattle, BattleAtaxiaUpdate} =
      btl_battle:ataxia_set_character
      (
         CharacterIX,
         UpdatedCharacter,
         CharacterAtaxiaUpdate,
         S0Battle
      ),

   TimelineItem =
      btl_turn_result:new_character_moved(CharacterIX, Path, NewLocation),

   S1Update = btl_character_turn_update:add_to_timeline(TimelineItem, S0Update),
   S2Update =
      btl_character_turn_update:ataxia_set_battle
      (
         UpdatedBattle,
         BattleAtaxiaUpdate,
         S1Update
      ),

   S2Update.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec handle
   (
      btl_action:type(),
      btl_character:type(),
      btl_character_turn_update:type()
   )
   ->
   (
      {'ok', btl_character_turn_update:type()}
      | {'events', list(btl_action:type()), btl_character_turn_update:type()}
   ).
handle (Action, Character, S0Update) ->
   Path = btl_action:get_path(Action),
   CharacterIX = btl_action:get_actor_index(Action),

   {
      NewLocation,
      RemainingPath,
      PathCost,
      Interruptions,
      HandledPath
   } =
      get_path_cost_and_destination(CharacterIX, Character, S0Update, Path),

   MovementPoints = get_movement_points(Action, Character),

   true = (MovementPoints >= PathCost),

   % [FIXME][IMPORTANT]: 'Path' will not be correct if there is an interruption.
   S1Update =
      commit_move(CharacterIX, Character, S0Update, HandledPath, NewLocation),

   case RemainingPath of
      [] -> {ok, S1Update};
      _ ->
         {events,
            (
               Interruptions
               ++
               [
                  btl_action:new_move
                  (
                     CharacterIX,
                     RemainingPath,
                     (MovementPoints - PathCost)
                  )
               ]
            ),
            S1Update
         }
   end.
