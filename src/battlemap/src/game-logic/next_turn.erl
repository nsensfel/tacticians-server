-module(next_turn).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export
(
   [
      update_if_needed/1
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec set_player_turn_to_next (battle:type()) -> battle:type().
set_player_turn_to_next (Battle) ->
   Players = battle:get_players(Battle),
   CurrentPlayerTurn = battle:get_current_player_turn(Battle),

   NextPlayerTurn = player_turn:next(array:size(Players), CurrentPlayerTurn),

   battle:set_current_player_turn(NextPlayerTurn, Battle).

-spec reset_next_player_timeline
   (
      battle:type()
   )
   -> {battle:type(), player:type()}.
reset_next_player_timeline (Battle) ->
   NextPlayerTurn = battle:get_current_player_turn(Battle),
   NextPlayerIX = player_turn:get_player_ix(NextPlayerTurn),
   NextPlayer = battle:get_player(NextPlayerIX, Battle),

   UpdatedNextPlayer = player:reset_timeline(NextPlayer),
   UpdatedBattle = battle:set_player(NextPlayerIX, UpdatedNextPlayer, Battle),

   {UpdatedBattle, UpdatedNextPlayer}.


-spec activate_next_players_characters
   (
      battle:type(),
      player:type()
   )
   -> {battle:type(), list(non_neg_integer())}.
activate_next_players_characters (Battle, NextPlayer) ->
   NextPlayerID = player:get_id(NextPlayer),
   CharacterInstances = battle:get_character_instances(Battle),

   {UpdatedCharacterInstances, ModifiedIXs} =
      array_util:mapiff
      (
         fun (CharacterInstance) ->
            Character = character_instance:get_character(CharacterInstance),
            (character:get_owner_id(Character) == NextPlayerID)
         end,
         fun (CharacterInstance) ->
            character_instance:set_is_active(true, CharacterInstance)
         end,
         CharacterInstances
      ),

   UpdatedBattle =
      battle:set_character_instances(UpdatedCharacterInstances, Battle),

   {UpdatedBattle, ModifiedIXs}.

-spec add_activation_updates
   (
      list(non_neg_integer()),
      character_turn_update:type()
   )
   -> character_turn_update:type().
add_activation_updates ([], Update) ->
   Update;
add_activation_updates ([IX|NextIXs], Update) ->
   % TODO: use DB update elements.
   add_activation_updates
   (
      NextIXs,
      character_turn_update:add_to_db(IX, Update)
   ).

-spec update (character_turn_update:type()) -> character_turn_update:type().
update (Update) ->
   Data = character_turn_update:get_data(Update),
   Battle = character_turn_data:get_battle(Data),

   S0Battle = set_player_turn_to_next(Battle),
   {S1Battle, NextPlayer} = reset_next_player_timeline(S0Battle),
   {S2Battle, ActivatedCharacterInstancesIX} =
      activate_next_players_characters(S1Battle, NextPlayer),

   S0Update = add_activation_updates(ActivatedCharacterInstancesIX, Update),

   UpdatedData = character_turn_data:set_battle(S2Battle, Data),

   character_turn_update:set_data(UpdatedData, S0Update).

-spec requires_update (character_turn_update:type()) -> boolean().
requires_update (Update) ->
   Data = character_turn_update:get_data(Update),
   Battle = character_turn_data:get_battle(Data),
   CharacterInstances = battle:get_character_instances(Battle),

   array_util:none(fun character_instance:get_is_active/1, CharacterInstances).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec update_if_needed
   (
      character_turn_update:type()
   )
   -> character_turn_update:type().
update_if_needed (Update) ->
   case requires_update(Update) of
      true -> update(Update);
      _ -> Update
   end.
