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
      shr_map:type(),
      list(shr_location:type()),
      list(shr_direction:enum()),
      non_neg_integer(),
      shr_location:type()
   )
   -> {shr_location:type(), non_neg_integer()}.
cross (_Map, _ForbiddenLocations, [], Cost, Location) ->
   {Location, Cost};
cross (Map, ForbiddenLocations, [Step|NextSteps], Cost, Location) ->
   NextLocation = shr_location:apply_direction(Step, Location),
   NextTileInstance = shr_map:get_tile_instance(NextLocation, Map),
   NextTileClassID = shr_tile:extract_main_class_id(NextTileInstance),
   NextTile = shr_tile:from_class_id(NextTileClassID),
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

   IsForbidden = false,

   cross(Map, ForbiddenLocations, NextSteps, NextCost, NextLocation).

-spec cross
   (
      shr_map:type(),
      list(shr_location:type()),
      list(shr_direction:enum()),
      shr_location:type()
   )
   -> {shr_location:type(), non_neg_integer()}.
cross (Map, ForbiddenLocations, Path, Location) ->
   cross(Map, ForbiddenLocations, Path, 0, Location).

-spec get_path_cost_and_destination
   (
      btl_character_turn_data:type(),
      list(shr_direction:type())
   )
   -> {non_neg_integer(), shr_location:type()}.
get_path_cost_and_destination (Data, Path) ->
   Character = btl_character_turn_data:get_character(Data),
   CharacterIX = btl_character_turn_data:get_character_ix(Data),
   Battle = btl_character_turn_data:get_battle(Data),
   Map = btl_battle:get_map(Battle),

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

   {NewLocation, Cost} =
      cross
      (
         Map,
         ForbiddenLocations,
         Path,
         btl_character:get_location(Character)
      ),

   {Cost, NewLocation}.

-spec assert_character_can_move
   (
      btl_character_turn_data:type(),
      non_neg_integer()
   )
   -> 'ok'.
assert_character_can_move (Data, Cost) ->
   CharacterData = btl_character_turn_data:get_character_current_data(Data),
   CharacterStats= btl_character_current_data:get_statistics(CharacterData),
   CharacterMovementPoints = shr_statistics:get_movement_points(CharacterStats),

   true = (Cost =< CharacterMovementPoints),

   ok.

-spec commit_move
   (
      btl_character_current_data:type(),
      btl_character_turn_update:type(),
      list(shr_direction:type()),
      shr_location:type()
   )
   -> btl_character_turn_update:type().
commit_move (PreviousCurrentData, Update, Path, NewLocation) ->
   Data = btl_character_turn_update:get_data(Update),
   Character = btl_character_turn_data:get_character(Data),
   CharacterIX = btl_character_turn_data:get_character_ix(Data),

   UpdatedCharacter = btl_character:set_location(NewLocation, Character),
   S0Data = btl_character_turn_data:set_character(UpdatedCharacter, Data),
   S1Data = btl_character_turn_data:refresh_character_current_data(S0Data),

   S0Update = btl_character_turn_update:set_data(S1Data, Update),
   S1Update =
      btl_turn_actions_stats_change:handle_max_health_changes
      (
         PreviousCurrentData,
         S0Update
      ),

   TimelineItem =
      btl_turn_result:new_character_moved(CharacterIX, Path, NewLocation),

   DBQuery =
      ataxic:update_field
      (
         btl_battle:get_characters_field(),
         ataxic_sugar:update_orddict_element
         (
            CharacterIX,
            ataxic:update_field
            (
               btl_character:get_location_field(),
               ataxic:constant(NewLocation)
            )
         )
      ),

   S2Update =
      btl_character_turn_update:add_to_timeline
      (
         TimelineItem,
         DBQuery,
         S1Update
      ),

   S2Update.

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
   CharacterCurrentData =
      btl_character_turn_data:get_character_current_data(Data),
   Path = btl_battle_action:get_path(BattleAction),

   {PathCost, NewLocation} = get_path_cost_and_destination(Data, Path),
   assert_character_can_move(Data, PathCost),

   commit_move(CharacterCurrentData, Update, Path, NewLocation).
