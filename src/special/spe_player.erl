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
   ok = ataxia_client:reserve(login_db, UsernameLC),
   ok = ataxia_client:reserve(login_db, EmailLC),

   ok.

-spec finalize_login (binary(), binary(), binary()) -> 'ok'.
finalize_login (UsernameLC, EmailLC, PlayerID) ->
   LoginUpdateQueryOps =
      ataxic:sequence_meta
      (
         [
            ataxic:update_value(ataxic:constant(PlayerID)),
            ataxic:update_read_permission
            (
               ataxic:constant(ataxia_security:any())
            ),
            ataxic:update_write_permission
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
         ataxia_security:allow_only(ataxia_security:any()),
         ataxia_security:allow_only(ataxia_security:user_from_id(PlayerID)),
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
         ataxia_security:allow_only(ataxia_security:any()),
         ataxia_security:allow_only(ataxia_security:user_from_id(PlayerID)),
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

   JanitorOnlyPermission =
      ataxia_security:allow_only(ataxia_security:janitor()),

   {ok, PlayerID} =
      ataxia_client:add
      (
         player_db,
         JanitorOnlyPermission,
         JanitorOnlyPermission,
         Player
      ),

   shr_janitor:new(player_db, PlayerID),

   InvID = generate_inventory(PlayerID),
   RosterID = generate_roster(PlayerID),

   PlayerUpdateQueryOps =
      ataxic:sequence_meta
      (
         [
            ataxic:update_value
            (
               ataxic:sequence
               (
                  [
                     ataxic:update_field
                     (
                        shr_player:get_id_field(),
                        ataxic:constant(PlayerID)
                     ),
                     ataxic:update_field
                     (
                        shr_player:get_inventory_id_field(),
                        ataxic:constant(InvID)
                     ),
                     ataxic:update_field
                     (
                        shr_player:get_roster_id_field(),
                        ataxic:constant(RosterID)
                     )
                  ]
               )
            ),
            ataxic:update_read_permission
            (
               ataxic:constant
               (
                  ataxia_security:allow_only(ataxia_security:any())
               )
            ),
            ataxic:update_write_permission
            (
               ataxic:constant
               (
                  ataxia_security:allow_only
                  (
                     ataxia_security:user_from_id(PlayerID)
                  )
               )
            )
         ]
      ),

   ok = finalize_login(UsernameLC, EmailLC, PlayerID),

   ok =
      ataxia_client:update
      (
         player_db,
         ataxia_security:janitor(),
         PlayerUpdateQueryOps,
         PlayerID
      ),


   Result = shr_player:set_id(PlayerID, Player),

   Result.
