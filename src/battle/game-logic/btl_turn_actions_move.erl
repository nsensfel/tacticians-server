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
-spec get_path_cost_and_destination
   (
      btl_character_turn_data:type(),
      list(btl_direction:type())
   )
   -> {non_neg_integer(), btl_location:type()}.
get_path_cost_and_destination (Data, Path) ->
   Character = btl_character_turn_data:get_character(Data),
   CharacterIX = btl_character_turn_data:get_character_ix(Data),
   Battle = btl_character_turn_data:get_battle(Data),
   Map = btl_battle:get_map(Battle),

   ForbiddenLocations =
      array:foldl
      (
         fun (IX, Char, Prev) ->
            IsAlive = btl_character:get_is_alive(Char),
            if
               (IX == CharacterIX) -> Prev;
               (not IsAlive) -> Prev;
               true -> [btl_character:get_location(Char)|Prev]
            end
         end,
         [],
         btl_battle:get_characters(Battle)
      ),

   {NewLocation, Cost} =
      btl_movement:cross
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
   Character = btl_character_turn_data:get_character(Data),
   CharacterStatistics = btl_character:get_statistics(Character),
   CharacterMovementPoints =
      shr_statistics:get_movement_points(CharacterStatistics),

   true = (Cost =< CharacterMovementPoints),

   ok.

-spec commit_move
   (
      btl_character_turn_update:type(),
      list(btl_direction:type()),
      btl_location:type()
   )
   -> btl_character_turn_update:type().
commit_move (Update, Path, NewLocation) ->
   Data = btl_character_turn_update:get_data(Update),
   Character = btl_character_turn_data:get_character(Data),
   CharacterIX = btl_character_turn_data:get_character_ix(Data),

   UpdatedCharacter = btl_character:set_location(NewLocation, Character),

   UpdatedData = btl_character_turn_data:set_character(UpdatedCharacter, Data),

   TimelineItem =
      btl_turn_result:new_character_moved(CharacterIX, Path, NewLocation),

   DBQuery =
      shr_db_query:update_indexed
      (
         btl_battle:get_characters_field(),
         CharacterIX,
         [
            shr_db_query:set_field
            (
               btl_character:get_location_field(),
               NewLocation
            )
         ]
      ),

   S0Update =
      btl_character_turn_update:add_to_timeline
      (
         TimelineItem,
         DBQuery,
         Update
      ),

   btl_character_turn_update:set_data(UpdatedData, S0Update).

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
   Path = btl_battle_action:get_path(BattleAction),

   {PathCost, NewLocation} = get_path_cost_and_destination(Data, Path),
   assert_character_can_move(Data, PathCost),

   commit_move(Update, Path, NewLocation).
