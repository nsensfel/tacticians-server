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
      btl_character_turn_update:type(),
      list(shr_direction:type())
   )
   ->
   {
      non_neg_integer(),
      shr_location:type(),
      btl_character_turn_update:type()
   }.
get_path_cost_and_destination (Update, Path) ->
   {S0Update, Character} = btl_character_turn_update:get_character(Update),
   {S1Update, Battle} = btl_character_turn_update:get_battle(S0Update),
   CharacterIX = btl_character_turn_update:get_character_ix(S1Update),
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

   {Cost, NewLocation, S1Update}.

-spec assert_character_can_move
   (
      btl_character:type(),
      non_neg_integer()
   )
   -> 'ok'.
assert_character_can_move (Char, Cost) ->
   CharacterMovementPoints =
      shr_statistics:get_movement_points
      (
         shr_character:get_statistics
         (
            btl_character:get_base_character(Char)
         )
      ),

   true = (Cost =< CharacterMovementPoints),

   ok.

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
   -> btl_character_turn_update:type().
handle (BattleAction, Update) ->
   {S0Update, Character} = btl_character_turn_update:get_character(Update),
   Path = btl_action:get_path(BattleAction),

   {PathCost, NewLocation, S1Update} =
      get_path_cost_and_destination(S0Update, Path),

   assert_character_can_move(Character, PathCost),

   commit_move(Character, S1Update, Path, NewLocation).
