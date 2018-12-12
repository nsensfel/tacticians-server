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
               ataxic:constant([ataxia_security:user(PlayerID)])
            )
         ]
      ),

   ok =
      ataxia_client:commit
      (
         login_db,
         ataxia_security:janitor(),
         LoginUpdateQueryOps,
         UsernameLC
      ),

   ok =
      ataxia_client:commit
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
      ataxia_client:insert
      (
         inventory_db,
         ataxia_security:any(),
         [ataxia_security:user(PlayerID)],
         Inventory
      ),

   InventoryID.

-spec generate_roster (ataxia_id:type()) -> ataxia_id:type().
generate_roster (PlayerID) ->
   Roster = rst_roster:new(PlayerID),
   {ok, RosterID} =
      ataxia_client:insert
      (
         roster_db,
         ataxia_security:any(),
         [ataxia_security:user(PlayerID)],
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
      ataxia_client:insert
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
      [
         shr_db_query:set_field(shr_player:get_id_field(), PlayerID),
         shr_db_query:set_field(shr_player:get_inventory_id_field(), InvID),
         shr_db_query:set_field(shr_player:get_roster_id_field(), RosterID),
         shr_db_query:set_read_permission(any),
         shr_db_query:set_write_permission([{user, PlayerID}])
      ],

   ok = finalize_login(UsernameLC, EmailLC, PlayerID),

   ok =
      shr_database:commit
      (
         shr_db_query:new(player_db, PlayerID, janitor, PlayerUpdateQueryOps)
      ),


   Result = shr_player:set_id(PlayerID, Player),

   Result.
