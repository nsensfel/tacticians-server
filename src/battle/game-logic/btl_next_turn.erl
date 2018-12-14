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
   -> {btl_battle:type(), ataxic:basic()}.
set_player_turn_to_next (Battle) ->
   Players = btl_battle:get_players(Battle),
   CurrentPlayerTurn = btl_battle:get_current_player_turn(Battle),

   NextPlayerTurn = btl_player_turn:next(Players, CurrentPlayerTurn),

   UpdatedBattle = btl_battle:set_current_player_turn(NextPlayerTurn, Battle),

   DBQuery =
      ataxic:on_field
      (
         btl_battle:get_current_player_turn_field(),
         ataxic:constant(NextPlayerTurn)
      ),

   {UpdatedBattle, DBQuery}.

-spec reset_next_player_timeline (btl_battle:type())
   -> {btl_battle:type(), btl_player:type(), ataxic:basic()}.
reset_next_player_timeline (Battle) ->
   NextPlayerTurn = btl_battle:get_current_player_turn(Battle),
   NextPlayerIX = btl_player_turn:get_player_ix(NextPlayerTurn),
   NextPlayer = btl_battle:get_player(NextPlayerIX, Battle),

   UpdatedNextPlayer = btl_player:reset_timeline(NextPlayer),
   UpdatedBattle =
      btl_battle:set_player(NextPlayerIX, UpdatedNextPlayer, Battle),

   DBQuery =
      ataxic:on_field
      (
         btl_battle:get_players_field(),
         ataxic_sugar:update_array_cell
         (
            NextPlayerIX,
            ataxic:on_field
            (
               btl_player:get_timeline_field(),
               ataxic:constant([])
            )
         )
      ),

   {UpdatedBattle, UpdatedNextPlayer, DBQuery}.


-spec activate_next_players_characters (btl_battle:type(), btl_player:type())
   -> {btl_battle:type(), ataxic:basic()}.
activate_next_players_characters (Battle, NextPlayer) ->
   NextPlayerIX = btl_player:get_index(NextPlayer),
   Characters = btl_battle:get_characters(Battle),

   {UpdatedCharacters, ModifiedIXs} =
      shr_array_util:mapiff
      (
         fun (Character) ->
            (btl_character:get_player_index(Character) == NextPlayerIX)
         end,
         fun (Character) ->
            btl_character:set_is_active(true, Character)
         end,
         Characters
      ),

   DBQuery =
      ataxic:on_field
      (
         btl_battle:get_characters_field(),
         ataxic:sequence
         (
            lists:map
            (
               fun (IX) ->
                  ataxic_sugar:update_array_cell
                  (
                     IX,
                     ataxic:on_field
                     (
                        btl_character:get_is_active_field(),
                        ataxic:constant(true)
                     )
                  )
               end,
               ModifiedIXs
            )
         )
      ),

   UpdatedBattle = btl_battle:set_characters(UpdatedCharacters, Battle),

   {UpdatedBattle, DBQuery}.

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
   {S2Battle, DBQuery2} =
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
         [DBQuery1,DBQuery2]
      ),

   S2Update.

-spec requires_update (btl_character_turn_update:type()) -> boolean().
requires_update (Update) ->
   Data = btl_character_turn_update:get_data(Update),
   Battle = btl_character_turn_data:get_battle(Data),
   Characters = btl_battle:get_characters(Battle),

   shr_array_util:none(fun btl_character:get_is_active/1, Characters).

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
