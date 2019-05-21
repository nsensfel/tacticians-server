-module(btl_turn_actions_move).
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
      list:foldl
      (
         fun (MarkerName, CurrentInterruptions) ->
            case shr_map:get_marker(MarkerName, Map) of
               {ok, Marker} ->
                  case shr_map_marker:interrupts_movement(PlayerIX, Marker) of
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
      btl_character_turn_update:type(),
      list(shr_direction:type())
   )
   ->
   {
      non_neg_integer(),
      shr_location:type(),
      list(shr_direction:type()),
      list(shr_map_marker:type()),
      btl_character_turn_update:type()
   }.
get_path_cost_and_destination (Update, Path) ->
   {S0Update, Character} = btl_character_turn_update:get_character(Update),
   {S1Update, Battle} = btl_character_turn_update:get_battle(S0Update),
   CharacterIX = btl_character_turn_update:get_character_ix(S1Update),
   Map = btl_battle:get_map(Battle),

   % FIXME: This is recalculated at every move action, despite there be no need
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

   {Cost, NewLocation, RemainingPath, Interruptions, S1Update}.

-spec get_movement_points
   (
      btl_action:type(),
      btl_character:type()
   )
   -> non_neg_integer().
get_movement_points (Action, Char) ->
   case btl_action:get_category(Action) of
      interrupted_move -> btl_action:get_movement_points(Action);
      _ ->
         shr_statistics:get_movement_points
         (
            shr_character:get_statistics
            (
               btl_character:get_base_character(Char)
            )
         )
   end.

-spec commit_move
   (
      btl_character:type(),
      btl_character_turn_update:type(),
      list(shr_direction:type()),
      shr_location:type()
   )
   -> btl_character_turn_update:type().
commit_move (Character, Update, Path, NewLocation) ->
   {S0Update, Battle} = btl_character_turn_update:get_battle(Update),
   Map = btl_battle:get_map(Battle),
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

   S1Update =
      btl_character_turn_update:ataxia_set_character
      (
         UpdatedCharacter,
         CharacterAtaxiaUpdate,
         S0Update
      ),

   TimelineItem =
      btl_turn_result:new_character_moved
      (
         btl_character_turn_update:get_character_ix(S1Update),
         Path,
         NewLocation
      ),

   S2Update = btl_character_turn_update:add_to_timeline(TimelineItem, S1Update),

   S2Update.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec handle
   (
      btl_action:type(),
      btl_character_turn_update:type()
   )
   ->
   (
      {'ok', btl_character_turn_update:type()}
      | {'events', list(btl_action:type()), btl_character_turn_update:type()}
   ).
handle (BattleAction, Update) ->
   {S0Update, Character} = btl_character_turn_update:get_character(Update),

   Path = btl_action:get_path(BattleAction),

   {PathCost, NewLocation, RemainingPath, Interruptions, S1Update} =
      get_path_cost_and_destination(S0Update, Path),

   MovementPoints = get_movement_points(BattleAction, Character),

   true = (MovementPoints >= PathCost),

   S2Update = commit_move(Character, S1Update, Path, NewLocation),

   case RemainingPath of
      [] -> {ok, S2Update};
      _ ->
         {events,
            (
               lists:foldl
               (
                  fun (Marker, CurrentActions) ->
                     (
                        btl_action:from_map_marker(Character, Marker)
                        ++
                        CurrentActions
                     )
                  end,
                  [],
                  Interruptions
               )
               ++
               [
                  btl_action:new_interrupted_move
                  (
                     RemainingPath,
                     (MovementPoints - PathCost)
                  )
               ]
            ),
            S2Update
         }
   end.
