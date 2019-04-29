-module(bnt_join_battle).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([generate/6, attempt/7]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% ROSTERS HANDLING %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec get_forbidden_locations
   (
      btl_battle:type()
   )
   -> ordsets:ordset(shr_location:type()).
get_forbidden_locations (Battle) ->
   orddict:fold
   (
      fun (_IX, Char, Set) ->
         ordsets:add_element(btl_character:get_location(Char), Set)
      end,
      ordsets:new(),
      btl_battle:get_characters(Battle)
   ).

-spec find_random_location
   (
      shr_map:type(),
      ordsets:ordset(shr_location:type())
   )
   -> {shr_location:type(), shr_tile:type()}.
find_random_location (Map, ForbiddenLocations) ->
   MapWidth = shr_map:get_width(Map),
   MapHeight = shr_map:get_height(Map),

   Candidate =
      {
         shr_roll:between(0, (MapWidth - 1)),
         shr_roll:between(0, (MapHeight - 1))
      },

   IsForbidden = ordsets:is_element(Candidate, ForbiddenLocations),

   case IsForbidden of
      true -> find_random_location(Map, ForbiddenLocations);

      _ ->
         Tile =
            shr_tile:from_id
            (
               shr_tile_instance:get_tile_id
               (
                  shr_map:get_tile_instance(Candidate, Map)
               )
            ),

         case (shr_tile:get_cost(Tile) > 200) of
            true -> find_random_location(Map, ForbiddenLocations);

            false -> {Candidate, Tile}
         end
   end.

-spec create_character
   (
      non_neg_integer(),
      shr_character:unresolved(),
      shr_map:type(),
      ordsets:ordset(shr_location:type())
   )
   -> btl_character:unresolved().
create_character (PlayerIX, RosterChar, Map, ForbiddenLocations) ->
   {Location, Tile} = find_random_location(Map, ForbiddenLocations),
   TileOmnimods = shr_tile:get_omnimods(Tile),

   ResolvedBaseChar = shr_character:resolve(TileOmnimods, RosterChar),

   % TODO: link rank to roster.
   Result = btl_character:new(PlayerIX, optional, Location, ResolvedBaseChar),

   btl_character:to_unresolved(Result).

-spec handle_characters
   (
      non_neg_integer(),
      shr_map:type(),
      ordsets:ordset(shr_location:type()),
      list(shr_character:unresolved()),
      btl_battle:type()
   )
   -> { btl_battle:type(), ataxic:basic() }.
handle_characters (PlayerIX, Map, UsedLocations, RosterCharacters, Battle) ->
   {_FinalUsedLocations, FinalBattle, FinalBattleAtaxicUpdates} =
      lists:foldl
      (
         fun
         (
            RosterCharacter,
            {CurrentUsedLocations, CurrentBattle, CurrentBattleAtaxicUpdates}
         )
         ->
            NewCharacterRef =
               create_character
               (
                  PlayerIX,
                  RosterCharacter,
                  Map,
                  CurrentUsedLocations
               ),

            {_NewCharacterIX, NewBattle, NewBattleAtaxiaUpdate} =
               btl_battle:ataxia_add_character(NewCharacterRef, CurrentBattle),

            {
               [
                  btl_character:get_location(NewCharacterRef)
                  |CurrentUsedLocations
               ],
               NewBattle,
               [NewBattleAtaxiaUpdate|CurrentBattleAtaxicUpdates]
            }
         end,
         {UsedLocations, Battle, []},
         RosterCharacters
      ),

   {FinalBattle, ataxic:optimize(ataxic:sequence(FinalBattleAtaxicUpdates))}.

-spec add_player
   (
      shr_player:id(),
      non_neg_integer(),
      shr_battle_summary:category(),
      btl_battle:type()
   )
   -> {btl_battle:type(), non_neg_integer(), ataxic:basic()}.
add_player (PlayerID, PlayerSummaryIX, PlayerSummaryCategory, Battle) ->

   {PlayerIX, Update}.

-spec add_characters
   (
      list(shr_character:unresolved()),
      non_neg_integer(),
      btl_battle:type()
   )
   -> {btl_battle:type(), ataxic:basic()}.
add_characters (RosterCharacters, PlayerIX, Battle) ->
   CurrentCharacters = btl_battle:get_characters(Battle),
   NextCharacterIX = orddict:size(CurrentCharacters),
   Map = btl_battle:get_map(Battle),

   ForbiddenLocations = get_forbidden_locations(Battle),

   {NewCharacters, CharactersUpdates} =
      handle_characters
      (
         RosterCharacters,
         PlayerIX,
         Map,
         ForbiddenLocations,
         NextCharacterIX,
         CurrentCharacters,
         []
      ),

   {UsedPortraitIDs, UsedWeaponIDs, UsedArmorIDs} =
      get_equipment_ids(RosterCharacters),

   OldPortraitIDs = btl_battle:get_used_portrait_ids(Battle),
   PortraitIDsUpdate =
      ataxic:update_field
      (
         btl_battle:get_used_portrait_ids_field(),
         update_ordset(UsedPortraitIDs, OldPortraitIDs)
      ),

   OldWeaponIDs = btl_battle:get_used_weapon_ids(Battle),
   WeaponIDsUpdate =
      ataxic:update_field
      (
         btl_battle:get_used_weapon_ids_field(),
         update_ordset(UsedWeaponIDs, OldWeaponIDs)
      ),

   OldArmorIDs = btl_battle:get_used_armor_ids(Battle),
   ArmorIDsUpdate =
      ataxic:update_field
      (
         btl_battle:get_used_armor_ids_field(),
         update_ordset(UsedArmorIDs, OldArmorIDs)
      ),

   S0Battle = btl_battle:set_characters(NewCharacters, Battle),
   S1Battle =
      btl_battle:set_used_armor_ids
      (
         ordsets:union(UsedArmorIDs, OldArmorIDs),
         btl_battle:set_used_weapon_ids
         (
            ordsets:union(UsedWeaponIDs, OldWeaponIDs),
            btl_battle:set_used_portrait_ids
            (
               ordsets:union(UsedPortraitIDs, OldPortraitIDs),
               S0Battle
            )
         )
      ),

   Update =
      ataxic:sequence
      (
         [
            ataxic:update_field
            (
               btl_battle:get_characters_field(),
               ataxic:sequence(CharactersUpdates)
            ),
            PortraitIDsUpdate,
            WeaponIDsUpdate,
            ArmorIDsUpdate
         ]
      ),

   {S1Battle, Update}.

-spec get_roster_characters
   (
      shr_player:id(),
      list(non_neg_integer())
   )
   -> list(shr_character:unresolved()).
get_roster_characters (PlayerID, SelectedRosterCharacterIXs) ->
   Player = shr_timed_cache:fetch(player_db, ataxia_security:any(), PlayerID),
   RosterID = shr_player:get_roster_id(Player),
   Roster =
      shr_timed_cache:fetch
      (
         roster_db,
         ataxia_security:user_from_id(PlayerID),
         RosterID
      ),

   RosterCharacters = rst_roster:get_characters(Roster),

   lists:map
   (
      fun (CharIX) ->
         orddict:fetch(CharIX, RosterCharacters)
      end,
      SelectedRosterCharacterIXs
   ).


%%%% STAGE 0: UPDATING THE PENDING BATTLE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec add_to_pending_battle
   (
      shr_player:id(),
      non_neg_integer(),
      shr_battle_summary:category(),
      list(non_neg_integer()),
      btl_pending_battle:type()
   )
   -> {btl_pending_battle:type(), ataxic:basic()}.
add_to_pending_battle
(
   PlayerID,
   PlayerSumIX,
   PlayerSumCategory,
   SelectedRosterCharacterIXs,
   PendingBattle
) ->
   Battle = btl_pending_battle:get_battle(PendingBattle),
   RemainingSlots =
      (
         btl_pending_battle:get_free_slots(PendingBattle)
         - length(SelectedRosterCharacterIXs)
      ),

   NewCharacters = get_roster_characters(PlayerID, SelectedRosterCharacterIXs),

   NewPlayer =
      btl_player:new
      (
         0,
         PlayerID,
         PlayerSumIX,
         PlayerSumCategory
      ),

   {PlayerIX, S0Battle, BattleAtaxiaUpdate} =
      btl_battle:add_player(NewPlayer, Battle),

   {S1Battle, BattleUpdate1} =
      add_characters(NewCharacters, PlayerIX, S0Battle),

   S0PendingBattle = btl_pending_battle:set_battle(S1Battle, PendingBattle),
   S1PendingBattle =
      btl_pending_battle:set_free_slots(RemainingSlots, S0PendingBattle),
   S2PendingBattle =
      btl_pending_battle:push_player_id(PlayerID, S1PendingBattle),
   S3PendingBattle =
      btl_pending_battle:push_player_summary_ix(PlayerIX, S2PendingBattle),

   Update =
      ataxic:sequence
      (
         [
            ataxic:update_field
            (
               btl_pending_battle:get_battle_field(),
               ataxic:sequence
               (
                  [
                     BattleUpdate0,
                     BattleUpdate1
                  ]
               )
            ),
            ataxic:update_field
            (
               btl_pending_battle:get_free_slots_field(),
               ataxic:constant(RemainingSlots)
            ),
            ataxic:update_field
            (
               btl_pending_battle:get_player_ids_field(),
               ataxic:list_cons(ataxic:constant(PlayerID))
            ),
            ataxic:update_field
            (
               btl_pending_battle:get_player_summary_ixs_field(),
               ataxic:list_cons(ataxic:constant(PlayerSumIX))
            )
         ]
      ),

   {S3PendingBattle, Update}.

%%%% STAGE -1: CREATING THE PENDING BATTLE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec generate_pending_battle
   (
      shr_player:id(),
      non_neg_integer(),
      shr_battle_summary:category(),
      shr_map:id(),
      list(non_neg_integer())
   )
   -> btl_pending_battle:type().
generate_pending_battle
(
   PlayerID,
   PlayerSumIX,
   PlayerSumCategory,
   MapID,
   SelectedRosterCharacterIXs
) ->
   Map =
      shr_timed_cache:fetch
      (
         map_db,
         ataxia_security:user_from_id(PlayerID),
         MapID
      ),

   Battle = btl_battle:new(Map),

   PendingBattle =
      btl_pending_battle:new
      (
         % TODO: More options than 1 vs N.
         (length(SelectedRosterCharacterIXs) * 2),
         Battle
      ),

   {S0PendingBattle, _AtaxicUpdate} =
      add_to_pending_battle
      (
         PlayerID,
         PlayerSumIX,
         PlayerSumCategory,
         SelectedRosterCharacterIXs,
         PendingBattle
      ),

   S0PendingBattle.

-spec repair_join_battle
   (
      shr_player:id(),
      non_neg_integer(),
      shr_battle_summary:category(),
      list(non_neg_integer()),
      btl_pending_battle:id(),
      btl_pending_battle:type()
   )
   -> {ok, btl_pending_battle:type()}.
repair_join_battle
(
   PlayerID,
   PlayerSumIX,
   PlayerSumCategory,
   RosterCharIXs,
   PBattleID,
   PBattle
) ->
   PlayerUser = ataxia_security:user_from_id(PlayerID),

   {S0PBattle, AtaxicUpdate} =
      add_to_pending_battle
      (
         PlayerID,
         PlayerSumIX,
         PlayerSumCategory,
         RosterCharIXs,
         PBattle
      ),

   ok =
      ataxia_client:update
      (
         pending_battle_db,
         PlayerUser,
         AtaxicUpdate,
         PBattleID
      ),

   {ok, S0PBattle}.

-spec repair_create_battle
   (
      shr_player:id(),
      non_neg_integer(),
      shr_battle_summary:category(),
      list(non_neg_integer()),
      btl_pending_battle:id(),
      shr_map:id()
   )
   -> {ok, btl_pending_battle:type()}.
repair_create_battle
(
   PlayerID,
   PlayerSumIX,
   PlayerSumCategory,
   RosterCharIXs,
   PBattleID,
   MapID
) ->
   NewPendingBattle =
      generate_pending_battle
      (
         PlayerID,
         PlayerSumIX,
         PlayerSumCategory,
         MapID,
         RosterCharIXs
      ),

   ok =
      ataxia_client:update
      (
         pending_battle_db,
         ataxia_security:user_from_id(PlayerID),
         ataxic:sequence_meta
         (
            [
               ataxic:update_value(ataxic:constant(NewPendingBattle)),
               ataxic:update_read_permission
               (
                  ataxic:constant(ataxia_security:allow_any())
               ),
               ataxic:update_write_permission
               (
                  ataxic:constant(ataxia_security:allow_any())
               )
            ]
         ),
         PBattleID
      ),

   {ok, NewPendingBattle}.

-spec repair_user_link
   (
      shr_player:id(),
      shr_battle_summary:mode(),
      shr_battle_summary:category(),
      non_neg_integer(),
      btl_pending_battle:id()
   )
   -> ok.
repair_user_link (PlayerID, Mode, Category, PBattleUserIX, PBattleID) ->
   PlayerUser = ataxia_security:user_from_id(PlayerID),
   BattleSummary =
      shr_battle_summary:new
      (
         PBattleID,
         <<"Test Battle">>,
         Mode,
         Category
      ),

   ok =
      ataxia_client:update
      (
         player_db,
         PlayerUser,
         ataxic:update_value
         (
            ataxic:update_field
            (
               (
                  case Category of
                     invasion -> shr_player:get_invasion_summaries_field();
                     event -> shr_player:get_event_summaries_field();
                     campaign -> shr_player:get_campaign_summaries_field()
                  end
               ),
               ataxic:apply_function
               (
                  orddict,
                  store,
                  [
                     ataxic:constant(PBattleUserIX),
                     ataxic:constant
                     (
                        BattleSummary
                     ),
                     ataxic:current_value()
                  ]
               )
            )
         ),
         PlayerID
      ).

-spec repair_generate_battle
   (
      btl_pending_battle:id(),
      btl_pending_battle:type()
   )
   -> {btl_battle:id(), btl_battle:type()}.
repair_generate_battle (PBattleID, PBattle) ->
   Battle = btl_pending_battle:get_battle(PBattle),
   {ok, BattleID} = ataxia_client:reserve(battle_db, ataxia_security:janitor()),

   BattlePermission =
      ataxia_security:allow
      (
         lists:map
         (
            fun ataxia_security:user_from_id/1,
            btl_pending_battle:get_player_ids(PBattle)
         )
      ),

   ok =
      ataxia_client:update
      (
         pending_battle_db,
         ataxia_security:admin(),
         ataxic:update_value
         (
            ataxic:update_field
            (
               btl_pending_battle:get_battle_id_field(),
               ataxic:constant(BattleID)
            )
         ),
         PBattleID
      ),

   ok =
      ataxia_client:update
      (
         battle_db,
         ataxia_security:admin(),
         ataxic:sequence_meta
         (
            [
               ataxic:update_read_permission(ataxic:constant(BattlePermission)),
               ataxic:update_write_permission
               (
                  ataxic:constant(BattlePermission)
               ),
               ataxic:update_value(ataxic:constant(Battle))
            ]
         ),
         BattleID
      ),

   {BattleID, Battle}.

-spec repair_battle_final_link_of_player
   (
      btl_battle:id(),
      btl_player:type()
   )
   -> ok.
repair_battle_final_link_of_player (BattleID, Player) ->
   SummaryField =
      (
         case btl_player:get_summary_category(Player) of
            invasion -> shr_player:get_invasion_summaries_field();
            event -> shr_player:get_event_summaries_field();
            campaign -> shr_player:get_campaign_summaries_field()
         end
      ),

   SummaryIX = btl_player:get_summary_index(Player),

   Update =
      ataxic:update_value
      (
         ataxic:update_field
         (
            SummaryField,
            ataxic:apply_function
            (
               orddict,
               store,
               [
                  ataxic:constant(SummaryIX),
                  ataxic:sequence
                  (
                     [
                        ataxic:apply_function
                        (
                           orddict,
                           fetch,
                           [
                              ataxic:constant(SummaryIX),
                              ataxic:current_value()
                           ]
                        ),
                        ataxic:update_field
                        (
                           shr_battle_summary:get_is_pending_field(),
                           ataxic:constant(false)
                        ),
                        ataxic:update_field
                        (
                           shr_battle_summary:get_id_field(),
                           ataxic:constant(BattleID)
                        ),
                        ataxic:update_field
                        (
                           shr_battle_summary:get_is_players_turn_field(),
                           ataxic:constant(btl_player:get_index(Player) == 0)
                        )
                     ]
                  ),
                  ataxic:current_value()
               ]
            )
         )
      ),

   ok =
      ataxia_client:update
      (
         player_db,
         ataxia_security:admin(),
         Update,
         btl_player:get_id(Player)
      ),

   ok.


-spec repair_battle_final_links
   (
      btl_pending_battle:id(),
      btl_battle:id(),
      btl_battle:type()
   )
   -> ok.
repair_battle_final_links (PendingBattleID, BattleID, Battle) ->

   Players = btl_battle:get_players(Battle),

   true =
      lists:all
      (
         fun ({_, Player}) ->
            (repair_battle_final_link_of_player(BattleID, Player) == ok)
         end,
         orddict:to_list(Players)
      ),

   ok =
      ataxia_client:remove
      (
         pending_battle_db,
         ataxia_security:admin(),
         PendingBattleID
      ),

   ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec generate
   (
      shr_player:id(),
      shr_battle_summary:mode(),
      shr_battle_summary:category(),
      non_neg_integer(),
      shr_map:id(),
      list(non_neg_integer())
   )
   -> 'ok'.
generate (PlayerID, Mode, Category, SummaryIX, MapID, RosterCharIXs) ->
   PlayerUser = ataxia_security:user_from_id(PlayerID),
   AnyoneAndMeAllowed =
      ataxia_security:add_access(PlayerUser, ataxia_security:allow_any()),

   {ok, NewPBattleID} =
      ataxia_client:reserve
      (
         pending_battle_db,
         AnyoneAndMeAllowed,
         AnyoneAndMeAllowed
      ),

%   BountyParams =
%      #bounty_params
%      {
%         player_id = PlayerID,
%         summary_ix = SummaryIX,
%         map_id = MapID,
%         roster_ixs = RosterCharIXs,
%         pending_battle_id = NewPBattleID
%      },

   % TODO: generate bounty.

   repair_create_battle
   (
      PlayerID,
      SummaryIX,
      Category,
      RosterCharIXs,
      NewPBattleID,
      MapID
   ),

   repair_user_link(PlayerID, Mode, Category, SummaryIX, NewPBattleID),

   ok.


-spec attempt
   (
      shr_player:id(),
      shr_battle_summary:mode(),
      shr_battle_summary:category(),
      non_neg_integer(),
      list(non_neg_integer()),
      btl_pending_battle:id(),
      btl_pending_battle:type()
   )
   -> 'ok'.
attempt
(
   PlayerID,
   Mode,
   Category,
   SummaryIX,
   SelectedRosterCharacterIXs,
   PendingBattleID,
   PendingBattle
) ->
%   BountyParams =
%      #bounty_params
%      {
%         player_id = PlayerID,
%         summary_ix = SummaryIX,
%         map_id = ataxia_id:null(),
%         roster_ixs = SelectedRosterCharacterIXs,
%         pending_battle_id = PendingBattleID
%      },

   % TODO: generate bounty.

   PlayerUser = ataxia_security:user_from_id(PlayerID),

   % Stage 0, optimized:
   {S0PendingBattle, AtaxicUpdate} =
      add_to_pending_battle
      (
         PlayerID,
         SummaryIX,
         Category,
         SelectedRosterCharacterIXs,
         PendingBattle
      ),

   ok =
      ataxia_client:update
      (
         pending_battle_db,
         PlayerUser,
         ataxic:update_value(AtaxicUpdate),
         PendingBattleID
      ),

   repair_user_link(PlayerID, Mode, Category, SummaryIX, PendingBattleID),

   case btl_pending_battle:get_free_slots(S0PendingBattle) of
      0 ->
         {BattleID, Battle} =
            repair_generate_battle(PendingBattleID, S0PendingBattle),
         ok =
            repair_battle_final_links
            (
               PendingBattleID,
               BattleID,
               Battle
            );

      _ -> ok
   end,

   ok.
