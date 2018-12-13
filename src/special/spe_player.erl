-module(spe_player).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([generate/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec reserve_login (binary(), binary()) -> 'ok'.
reserve_login (UsernameLC, EmailLC) ->
   shr_janitor:new(login_db, UsernameLC),
   shr_janitor:new(login_db, EmailLC),

   ok = ataxia_client:reserve(login_db, ataxia_security:janitor(), UsernameLC),
   ok = ataxia_client:reserve(login_db, ataxia_security:janitor(), EmailLC),

   ok.

-spec finalize_login (binary(), binary(), binary()) -> 'ok'.
finalize_login (UsernameLC, EmailLC, PlayerID) ->
   LoginUpdateQueryOps =
      ataxic:sequence_meta
      (
         [
            ataxic:value(ataxic:constant(PlayerID)),
            ataxic:read_permission(ataxic:constant(ataxia_security:any())),
            ataxic:write_permission
            (
               ataxic:constant([ataxia_security:user_from_id(PlayerID)])
            )
         ]
      ),

   ok =
      ataxia_client:update
      (
         login_db,
         ataxia_security:janitor(),
         LoginUpdateQueryOps,
         UsernameLC
      ),

   ok =
      ataxia_client:update
      (
         login_db,
         ataxia_security:janitor(),
         LoginUpdateQueryOps,
         EmailLC
      ),

   'ok'.

-spec generate_inventory (ataxia_id:type()) -> ataxia_id:type().
generate_inventory (PlayerID) ->
   Inventory = shr_inventory:new(PlayerID),

   {ok, InventoryID} =
      ataxia_client:add
      (
         inventory_db,
         ataxia_security:any(),
         [ataxia_security:user_from_id(PlayerID)],
         Inventory
      ),

   InventoryID.

-spec generate_roster (ataxia_id:type()) -> ataxia_id:type().
generate_roster (PlayerID) ->
   Roster = rst_roster:new(PlayerID),
   {ok, RosterID} =
      ataxia_client:add
      (
         roster_db,
         ataxia_security:any(),
         [ataxia_security:user_from_id(PlayerID)],
         Roster
      ),

   RosterID.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec generate (binary(), binary(), binary()) -> shr_player:type().
generate (Username, Password, Email) ->
   UsernameLC = string:lowercase(Username),
   EmailLC = string:lowercase(Email),

   ok = reserve_login(UsernameLC, EmailLC),

   Player = shr_player:new(<<"">>, Username, Password, Email),

   {ok, PlayerID} =
      ataxia_client:add
      (
         player_db,
         ataxia_security:janitor(),
         ataxia_security:janitor(),
         Player
      ),

   shr_janitor:new(player_db, PlayerID),

   InvID = generate_inventory(PlayerID),
   RosterID = generate_roster(PlayerID),

   PlayerUpdateQueryOps =
      ataxic:sequence_meta
      (
         [
            ataxic:value
            (
               ataxic:sequence
               (
                  [
                     ataxic:on_field
                     (
                        shr_player:get_id_field(),
                        ataxic:constant(PlayerID)
                     ),
                     ataxic:on_field
                     (
                        shr_player:get_inventory_id_field(),
                        ataxic:constant(InvID)
                     ),
                     ataxic:on_field
                     (
                        shr_player:get_roster_id_field(),
                        ataxic:constant(RosterID)
                     )
                  ]
               )
            ),
            ataxic:read_permission(ataxic:constant(ataxia_security:any())),
            ataxic:write_permission
            (
               ataxic:constant([ataxia_security:user_from_id(PlayerID)])
            )
         ]
      ),

   ok = finalize_login(UsernameLC, EmailLC, PlayerID),

   ok =
      ataxia:update
      (
         player_db,
         ataxia_security:janitor(),
         PlayerUpdateQueryOps,
         PlayerID
      ),


   Result = shr_player:set_id(PlayerID, Player),

   Result.
