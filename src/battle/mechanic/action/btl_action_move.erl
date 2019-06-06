-module(btl_action_move).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
-spec cross
   (
      non_neg_integer(),
      shr_map:type(),
      list(shr_location:type()),
      list(shr_direction:enum()),
      non_neg_integer(),
      shr_location:type()
   )
   ->
   {
      shr_location:type(),
      list(shr_direction:type()),
      non_neg_integer(),
      list(shr_map_marker:type())
   }.
cross (_PlayerIX, _Map, _ForbiddenLocations, [], Cost, Location) ->
   {Location, [], Cost, []};
cross (PlayerIX, Map, ForbiddenLocations, [Step|NextSteps], Cost, Location) ->
   NextLocation = shr_location:apply_direction(Step, Location),
   NextTileInstance = shr_map:get_tile_instance(NextLocation, Map),
   NextTileClassID = shr_tile_instance:get_tile_id(NextTileInstance),
   NextTile = shr_tile:from_id(NextTileClassID),
   NextCost = (Cost + shr_tile:get_cost(NextTile)),
   IsForbidden =
      lists:foldl
      (
         fun (ForbiddenLocation, Prev) ->
            (Prev or (NextLocation == ForbiddenLocation))
         end,
         false,
         ForbiddenLocations
      ),

   false = IsForbidden,

   Interruptions =
      lists:foldl
      (
         fun (MarkerName, CurrentInterruptions) ->
            case shr_map:get_marker(MarkerName, Map) of
               {ok, Marker} ->
                  case
                     shr_map_marker:interrupts_movement(PlayerIX, Marker)
                  of
                     true -> [Marker|CurrentInterruptions];
                     _ -> CurrentInterruptions
                  end;

               error ->
                  %% TODO: Error.
                  CurrentInterruptions
            end
         end,
         [],
         shr_tile_instance:get_triggers(NextTileInstance)
      ),

   case Interruptions of
      [] ->
         cross
         (
            PlayerIX,
            Map,
            ForbiddenLocations,
            NextSteps,
            NextCost,
            NextLocation
         );

      _ -> {NextLocation, NextSteps, NextCost, Interruptions}
   end.

-spec cross
   (
      non_neg_integer(),
      shr_map:type(),
      list(shr_location:type()),
      list(shr_direction:enum()),
      shr_location:type()
   )
   ->
   {
      shr_location:type(),
      list(shr_direction:type()),
      non_neg_integer(),
      list(shr_map_marker:type())
   }.
cross (PlayerIX, Map, ForbiddenLocations, Path, Location) ->
   cross(PlayerIX, Map, ForbiddenLocations, Path, 0, Location).

-spec get_path_cost_and_destination
   (
      non_neg_integer(),
      btl_character:type(),
      btl_character_turn_update:type(),
      list(shr_direction:type())
   )
   ->
   {
      non_neg_integer(),
      shr_location:type(),
      list(shr_direction:type()),
      list(shr_map_marker:type())
   }.
get_path_cost_and_destination (CharacterIX, Character, Update, Path) ->
   Battle = btl_character_turn_update:get_battle(Update),
   Map = btl_battle:get_map(Battle),

   % [TODO][OPTIMIZATION] Redundant calculations.
   % This is recalculated at every move action, despite there be no need
   % to: The client will not allow the character to go somewhere that would
   % only be freed because of an event.
   ForbiddenLocations =
      orddict:fold
      (
         fun (IX, Char, Prev) ->
            IsAlive = btl_character:get_is_alive(Char),
            if
               (IX == CharacterIX) -> Prev;
               (not IsAlive) -> Prev;
               true ->
                  ordsets:add_element(btl_character:get_location(Char), Prev)
            end
         end,
         ordsets:new(),
         btl_battle:get_characters(Battle)
      ),

   {NewLocation, RemainingPath, Cost, Interruptions} =
      cross
      (
         btl_character:get_player_index(Character),
         Map,
         ForbiddenLocations,
         Path,
         btl_character:get_location(Character)
      ),

   {Cost, NewLocation, RemainingPath, Interruptions}.

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

   {PathCost, NewLocation, RemainingPath, Interruptions} =
      get_path_cost_and_destination(CharacterIX, Character, S0Update, Path),

   MovementPoints = get_movement_points(Action, Character),

   true = (MovementPoints >= PathCost),

   S1Update = commit_move(CharacterIX, Character, S0Update, Path, NewLocation),

   case RemainingPath of
      [] -> {ok, S1Update};
      _ ->
         {events,
            (
               lists:foldl
               (
                  fun (Marker, CurrentActions) ->
                     (
                        btl_action:from_map_marker
                        (
                           CharacterIX,
                           Character,
                           Marker
                        )
                        ++
                        CurrentActions
                     )
                  end,
                  [],
                  Interruptions
               )
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
