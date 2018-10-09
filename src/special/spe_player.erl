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

   ok = shr_database:reserve(login_db, UsernameLC, janitor),
   ok = shr_database:reserve(login_db, EmailLC, janitor),

   ok.

-spec finalize_login (binary(), binary(), binary()) -> 'ok'.
finalize_login (UsernameLC, EmailLC, PlayerID) ->
   LoginUpdateQueryOps =
      [
         shr_db_query:set_value(PlayerID),
         shr_db_query:set_read_permission(any),
         shr_db_query:set_write_permission([{user, PlayerID}])
      ],

   ok =
      shr_database:commit
      (
         shr_db_query:new(login_db, UsernameLC, janitor, LoginUpdateQueryOps)
      ),

   ok =
      shr_database:commit
      (
         shr_db_query:new(login_db, EmailLC, janitor, LoginUpdateQueryOps)
      ),

   'ok'.

-spec generate_inventory (binary()) -> binary().
generate_inventory (PlayerID) ->
   Inventory = shr_inventory:new(PlayerID),
   {ok, InventoryID} =
      shr_database:insert
      (
         inventory_db,
         [{user, PlayerID}],
         [{user, PlayerID}],
         Inventory
      ),

   InventoryID.

-spec generate_roster (binary()) -> binary().
generate_roster (PlayerID) ->
   Roster = rst_roster:new(PlayerID),
   {ok, RosterID} =
      shr_database:insert
      (
         roster_db,
         [{user, PlayerID}],
         [{user, PlayerID}],
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

   {ok, PlayerID} = shr_database:insert(player_db, janitor, janitor, Player),


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
