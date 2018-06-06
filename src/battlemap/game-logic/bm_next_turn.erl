-module(bm_next_turn).
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
-spec set_player_turn_to_next (bm_battle:type()) -> bm_battle:type().
set_player_turn_to_next (Battle) ->
   Players = bm_battle:get_players(Battle),
   CurrentPlayerTurn = bm_battle:get_current_player_turn(Battle),

   NextPlayerTurn = bm_player_turn:next(array:size(Players), CurrentPlayerTurn),

   bm_battle:set_current_player_turn(NextPlayerTurn, Battle).

-spec reset_next_player_timeline
   (
      bm_battle:type()
   )
   -> {bm_battle:type(), bm_player:type()}.
reset_next_player_timeline (Battle) ->
   NextPlayerTurn = bm_battle:get_current_player_turn(Battle),
   NextPlayerIX = bm_player_turn:get_player_ix(NextPlayerTurn),
   NextPlayer = bm_battle:get_player(NextPlayerIX, Battle),

   UpdatedNextPlayer = bm_player:reset_timeline(NextPlayer),
   UpdatedBattle =
      bm_battle:set_player(NextPlayerIX, UpdatedNextPlayer, Battle),

   {UpdatedBattle, UpdatedNextPlayer}.


-spec activate_next_players_characters
   (
      bm_battle:type(),
      bm_player:type()
   )
   -> {bm_battle:type(), list(non_neg_integer())}.
activate_next_players_characters (Battle, NextPlayer) ->
   NextPlayerID = bm_player:get_id(NextPlayer),
   Characters = bm_battle:get_characters(Battle),

   {UpdatedCharacters, ModifiedIXs} =
      array_util:mapiff
      (
         fun (Character) ->
            (bm_character:get_owner_id(Character) == NextPlayerID)
         end,
         fun (Character) ->
            bm_character:set_is_active(true, Character)
         end,
         Characters
      ),

   UpdatedBattle = bm_battle:set_characters(UpdatedCharacters, Battle),

   {UpdatedBattle, ModifiedIXs}.

-spec add_activation_updates
   (
      list(non_neg_integer()),
      bm_character_turn_update:type()
   )
   -> bm_character_turn_update:type().
add_activation_updates ([], Update) ->
   Update;
add_activation_updates ([IX|NextIXs], Update) ->
   add_activation_updates
   (
      NextIXs,
      bm_character_turn_update:add_to_db
      (
         sh_db_query:update_indexed
         (
            bm_battle:get_characters_field(),
            IX,
            [sh_db_query:set_field(bm_character:get_active_field(), true)]
         ),
         Update
      )
   ).

-spec update
   (
      bm_character_turn_update:type()
   )
   -> bm_character_turn_update:type().
update (Update) ->
   Data = bm_character_turn_update:get_data(Update),
   Battle = bm_character_turn_data:get_battle(Data),

   S0Battle = set_player_turn_to_next(Battle),
   {S1Battle, NextPlayer} = reset_next_player_timeline(S0Battle),
   {S2Battle, ActivatedCharactersIX} =
      activate_next_players_characters(S1Battle, NextPlayer),

   S0Update = add_activation_updates(ActivatedCharactersIX, Update),

   UpdatedData = bm_character_turn_data:set_battle(S2Battle, Data),

   bm_character_turn_update:set_data(UpdatedData, S0Update).

-spec requires_update (bm_character_turn_update:type()) -> boolean().
requires_update (Update) ->
   Data = bm_character_turn_update:get_data(Update),
   Battle = bm_character_turn_data:get_battle(Data),
   Characters = bm_battle:get_characters(Battle),

   array_util:none(fun bm_character:get_is_active/1, Characters).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec update_if_needed
   (
      bm_character_turn_update:type()
   )
   -> character_turn_update:type().
update_if_needed (Update) ->
   case requires_update(Update) of
      true -> update(Update);
      _ -> Update
   end.
