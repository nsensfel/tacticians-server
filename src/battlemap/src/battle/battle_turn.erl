-module(battle_turn).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export
(
   [
      handle_post_play/1,
      store_timeline/2
   ]
).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec activate_relevant_character_instances
   (
      list(non_neg_integer()),
      array:array(character_instance:struct()),
      player:id(),
      (-1 | non_neg_integer())
   )
   -> {list(non_neg_integer()), array:array(character_instance:struct())}.
activate_relevant_character_instances (IXs, CharacterInstances, _Owner, -1) ->
   {IXs, CharacterInstances};
activate_relevant_character_instances (IXs, CharacterInstances, Owner, IX) ->
   CharacterInstance = array:get(IX, CharacterInstances),
   Character = character_instance:get_character(CharacterInstance),
   case character:get_owner_id(Character) of
      OwnerID when (OwnerID == Owner) ->
         activate_relevant_character_instances
         (
            [IX|IXs],
            array:set
            (
               IX,
               character_instance:set_is_active(true, CharacterInstance),
               CharacterInstances
            ),
            Owner,
            (IX - 1)
         );

      _ ->
         activate_relevant_character_instances
         (
            IXs,
            CharacterInstances,
            Owner,
            (IX - 1)
         )
   end.

-spec start_next_players_turn (battle:struct()) ->
   {list(non_neg_integer()), battle:struct()}.
start_next_players_turn (Battle) ->
   Players = battle:get_players(Battle),
   PlayerTurn = battle:get_current_player_turn(Battle),
   CurrentPlayerIX = player_turn:get_player_ix(PlayerTurn),
   CurrentTurnNumber = player_turn:get_number(PlayerTurn),
   CharacterInstances = battle:get_character_instances(Battle),

   NextPlayerIX = ((CurrentPlayerIX + 1) rem (array:size(Players))),
   NextPlayerTurn =
      player_turn:new
      (
         case NextPlayerIX of
            0 -> (CurrentTurnNumber + 1);
            _ -> CurrentTurnNumber
         end,
         NextPlayerIX
      ),

   NextPlayer = array:get(NextPlayerIX, Players),
   UpdatedNextPlayer = player:reset_timeline(NextPlayer),

   {ActivatedCharacterInstanceIXs, UpdatedCharacterInstances} =
      activate_relevant_character_instances
      (
         [],
         CharacterInstances,
         player:get_id(NextPlayer),
         (array:size(CharacterInstances) - 1)
      ),
   UpdatedBattle =
      battle:set_player
      (
         NextPlayerIX,
         UpdatedNextPlayer,
         battle:set_character_instances
         (
            UpdatedCharacterInstances,
            battle:set_current_player_turn
            (
               NextPlayerTurn,
               Battle
            )
         )
      ),
   % TODO: have a diff operation for the player's timeline being reset. 
   {ActivatedCharacterInstanceIXs, UpdatedBattle}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec store_timeline
   (
      list(any()),
      battle:struct()
   )
   -> battle:struct().
store_timeline (TurnEffects, Battle) ->
   PlayerTurn = battle:get_current_player_turn(Battle),
   PlayerIX = player_turn:get_player_ix(PlayerTurn),
   Player = battle:get_player(PlayerIX, Battle),

   UpdatedPlayer = player:add_to_timeline(TurnEffects, Player),

   battle:set_player(PlayerIX, UpdatedPlayer, Battle).


-spec handle_post_play (battle:struct())
   -> {database_diff:struct(), battle:struct()}.
handle_post_play (Battle) ->
   CharacterInstances = battle:get_character_instances(Battle),

   AnActiveCharacterInstanceRemains =
      array:foldl
      (
         fun (_IX, CharacterInstance, Prev) ->
            (Prev or character_instance:get_is_active(CharacterInstance))
         end,
         false,
         CharacterInstances
      ),

   case AnActiveCharacterInstanceRemains of
      true ->
         io:format("~nThere are still active characters.~n"),
         {[], Battle};

      false ->
         io:format("~nThere are no more active characters.~n"),
         {UpdatedCharacterInstanceIXs, UpdatedBattle} =
            start_next_players_turn(Battle),
         {
            lists:map
            (
               fun (IX) ->
                  {set, character_instance, IX, is_active, true}
               end,
               UpdatedCharacterInstanceIXs
            ),
            UpdatedBattle
         }
   end.
