-module(bnt_generate_player).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([attempt/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec reserve_login (binary(), binary()) -> 'ok'.
reserve_login (UsernameLC, EmailLC) ->
   Anyone = ataxia_security:allow_any(),
   ok = ataxia_client:reserve_at(login_db, Anyone, Anyone, UsernameLC),

   case EmailLC of
      <<"">> -> ok;
      _ -> ok = ataxia_client:reserve_at(login_db, Anyone, Anyone, EmailLC)
   end,

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
               ataxic:constant
               (
                  ataxia_security:allow_only
                  (
                     ataxia_security:any()
                  )
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

   ok =
      ataxia_client:update
      (
         login_db,
         ataxia_security:janitor(),
         LoginUpdateQueryOps,
         UsernameLC
      ),

   case EmailLC of
      <<"">> -> ok;
      _ ->
         ok =
            ataxia_client:update
            (
               login_db,
               ataxia_security:janitor(),
               LoginUpdateQueryOps,
               EmailLC
            )
   end,


   'ok'.

-spec generate_inventory (shr_player:id()) -> shr_inventory:id().
generate_inventory (PlayerID) ->
   Inventory = shr_inventory:default(),

   {ok, InventoryID} =
      ataxia_client:add
      (
         inventory_db,
         ataxia_security:allow_only(ataxia_security:any()),
         ataxia_security:allow_only(ataxia_security:user_from_id(PlayerID)),
         Inventory
      ),

   InventoryID.

-spec generate_roster (shr_player:id()) -> rst_roster:id().
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
-spec attempt
   (
      binary(),
      binary(),
      binary()
   )
   -> {shr_player:id(), shr_player:type()}.
attempt (Username, Password, Email) ->
   % FIXME: still requires a bounty.
   UsernameLC = string:lowercase(Username),
   EmailLC = string:lowercase(Email),

   ok = reserve_login(UsernameLC, EmailLC),

   Player = shr_player:new(Username, Password, Email),

   AnyoneHasAccess = ataxia_security:allow_any(),

   {ok, PlayerID} =
      ataxia_client:reserve
      (
         player_db,
         AnyoneHasAccess,
         AnyoneHasAccess
      ),

   shr_janitor:new(player_db, PlayerID),

   InvID = generate_inventory(PlayerID),
   RosterID = generate_roster(PlayerID),
   S0Player =
      shr_player:set_inventory_id
      (
         InvID,
         shr_player:set_roster_id(RosterID, Player)
      ),

   PlayerUpdateQueryOps =
      ataxic:sequence_meta
      (
         [
            ataxic:update_value(ataxic:constant(S0Player)),
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
         ataxia_security:user_from_id(PlayerID),
         PlayerUpdateQueryOps,
         PlayerID
      ),

   {PlayerID, S0Player}.
