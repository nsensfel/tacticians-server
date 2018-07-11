-module(btl_next_turn).
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
-spec set_player_turn_to_next (btl_battle:type())
   -> {btl_battle:type(), sh_db_query:op()}.
set_player_turn_to_next (Battle) ->
   Players = btl_battle:get_players(Battle),
   CurrentPlayerTurn = btl_battle:get_current_player_turn(Battle),

   NextPlayerTurn = btl_player_turn:next(Players, CurrentPlayerTurn),

   UpdatedBattle = btl_battle:set_current_player_turn(NextPlayerTurn, Battle),

   DBQuery =
      sh_db_query:set_field
      (
         btl_battle:get_current_player_turn_field(),
         NextPlayerTurn
      ),

   {UpdatedBattle, DBQuery}.

-spec reset_next_player_timeline (btl_battle:type())
   -> {btl_battle:type(), btl_player:type(), sh_db_query:op()}.
reset_next_player_timeline (Battle) ->
   NextPlayerTurn = btl_battle:get_current_player_turn(Battle),
   NextPlayerIX = btl_player_turn:get_player_ix(NextPlayerTurn),
   NextPlayer = btl_battle:get_player(NextPlayerIX, Battle),

   UpdatedNextPlayer = btl_player:reset_timeline(NextPlayer),
   UpdatedBattle =
      btl_battle:set_player(NextPlayerIX, UpdatedNextPlayer, Battle),

   DBQuery =
      sh_db_query:update_indexed
      (
         btl_battle:get_players_field(),
         NextPlayerIX,
         [ sh_db_query:set_field(btl_player:get_timeline_field(), []) ]
      ),

   {UpdatedBattle, UpdatedNextPlayer, DBQuery}.


-spec activate_next_players_characters (btl_battle:type(), btl_player:type())
   -> {btl_battle:type(), list(sh_db_query:op())}.
activate_next_players_characters (Battle, NextPlayer) ->
   NextPlayerIX = btl_player:get_index(NextPlayer),
   Characters = btl_battle:get_characters(Battle),

   {UpdatedCharacters, ModifiedIXs} =
      sh_array_util:mapiff
      (
         fun (Character) ->
            (btl_character:get_player_index(Character) == NextPlayerIX)
         end,
         fun (Character) ->
            btl_character:set_is_active(true, Character)
         end,
         Characters
      ),

   DBQueries =
      lists:map
      (
         fun (IX) ->
            sh_db_query:update_indexed
            (
               btl_battle:get_characters_field(),
               IX,
               [
                  sh_db_query:set_field
                  (
                     btl_character:get_is_active_field(),
                     true
                  )
               ]
            )
         end,
         ModifiedIXs
      ),

   UpdatedBattle = btl_battle:set_characters(UpdatedCharacters, Battle),

   {UpdatedBattle, DBQueries}.

-spec update
   (
      btl_character_turn_update:type()
   )
   -> btl_character_turn_update:type().
update (Update) ->
   Data = btl_character_turn_update:get_data(Update),
   Battle = btl_character_turn_data:get_battle(Data),

   {S0Battle, DBQuery0} = set_player_turn_to_next(Battle),
   {S1Battle, NextPlayer, DBQuery1} = reset_next_player_timeline(S0Battle),
   {S2Battle, DBQueries} =
      activate_next_players_characters(S1Battle, NextPlayer),

   S0Data = btl_character_turn_data:set_battle(S2Battle, Data),
   S0Update =
      btl_character_turn_update:add_to_timeline
      (
         btl_turn_result:new_player_turn_started
         (
            btl_player:get_index(NextPlayer)
         ),
         DBQuery0,
         Update
      ),

   S1Update = btl_character_turn_update:set_data(S0Data, S0Update),

   S2Update =
      lists:foldl
      (
         fun btl_character_turn_update:add_to_db/2,
         S1Update,
         [DBQuery1|DBQueries]
      ),

   S2Update.

-spec requires_update (btl_character_turn_update:type()) -> boolean().
requires_update (Update) ->
   Data = btl_character_turn_update:get_data(Update),
   Battle = btl_character_turn_data:get_battle(Data),
   Characters = btl_battle:get_characters(Battle),

   sh_array_util:none(fun btl_character:get_is_active/1, Characters).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec update_if_needed
   (
      btl_character_turn_update:type()
   )
   -> btl_character_turn_update:type().
update_if_needed (Update) ->
   case requires_update(Update) of
      true -> update(Update);
      _ -> Update
   end.
