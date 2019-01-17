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

%%%% USED IDS COLLECTION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec update_ordset
   (
      ordsets:ordset(any()),
      ordsets:ordset(any())
   )
   -> ataxic:basic().
update_ordset (New, Old) ->
   AddedElements = ordsets:subtract(New, Old),

   ataxic:sequence
   (
      lists:map
      (
         fun (V) ->
            ataxic:apply_function
            (
               ordsets,
               add_element,
               [
                  ataxic:constant(V),
                  ataxic:current_value()
               ]
            )
         end,
         ordsets:to_list(AddedElements)
      )
   ).

-spec get_equipment_ids
   (
      list(rst_character:type())
   )
   ->
   {
      ordsets:ordset(shr_portrait:id()),
      ordsets:ordset(shr_weapon:id()),
      ordsets:ordset(shr_armor:id())
   }.
get_equipment_ids (Characters) ->
   {
      UsedPortraitIDs,
      UsedWeaponIDs,
      UsedArmorIDs
   } =
      lists:foldl
      (
         fun (Character, {UPIDs, UWIDs, UAIDs}) ->
            {MWpID, SWpID} = rst_character:get_weapon_ids(Character),
            AID = rst_character:get_armor_id(Character),
            PID = rst_character:get_portrait_id(Character),
            {
               ordsets:add_element(PID, UPIDs),
               ordsets:add_element(MWpID, ordsets:add_element(SWpID, UWIDs)),
               ordsets:add_element(AID, UAIDs)
            }
         end,
         {ordsets:new(), ordsets:new(), ordsets:new()},
         Characters
      ),

   {UsedPortraitIDs, UsedWeaponIDs, UsedArmorIDs}.


%%%% ROSTERS HANDLING %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec get_forbidden_locations
   (
      btl_battle:type()
   )
   -> ordsets:ordset(btl_location:type()).
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
      btl_map:type(),
      ordsets:ordset(btl_location:type())
   )
   -> {btl_location:type(), shr_tile:type()}.
find_random_location (Map, ForbiddenLocations) ->
   MapWidth = btl_map:get_width(Map),
   MapHeight = btl_map:get_height(Map),

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
            shr_tile:from_class_id
            (
               shr_tile:extract_main_class_id
               (
                  btl_map:get_tile_instance(Candidate, Map)
               )
            ),

         case (shr_tile:get_cost(Tile) > 200) of
            true -> find_random_location(Map, ForbiddenLocations);

            false -> {Candidate, Tile}
         end
   end.

-spec get_glyphs_omnimods (rst_character:type()) -> shr_omnimods:type().
get_glyphs_omnimods (RosterChar) ->
   GlyphBoardID = rst_character:get_glyph_board_id(RosterChar),
   GlyphIDs = rst_character:get_glyph_ids(RosterChar),
   GlyphBoard = shr_glyph_board:from_id(GlyphBoardID),
   Glyphs = lists:map(fun shr_glyph:from_id/1, GlyphIDs),
   case shr_glyph_board:get_omnimods_with_glyphs(Glyphs, GlyphBoard) of
      {ok, Result} -> Result;
      error -> shr_omnimods:new([], [], [], [])
   end.

-spec create_character
   (
      non_neg_integer(),
      rst_character:type(),
      btl_map:type(),
      ordsets:ordset(btl_location:type())
   )
   -> btl_character:type().
create_character (PlayerIX, RosterChar, Map, ForbiddenLocations) ->
   {Location, Tile} = find_random_location(Map, ForbiddenLocations),
   TileOmnimods = shr_tile:get_omnimods(Tile),
   GlyphsOmnimods = get_glyphs_omnimods(RosterChar),

   Result =
      btl_character:new
      (
         PlayerIX,
         rst_character:get_name(RosterChar),
         optional, % TODO: link this to roster.
         GlyphsOmnimods,
         rst_character:get_portrait_id(RosterChar),
         rst_character:get_weapon_ids(RosterChar),
         rst_character:get_armor_id(RosterChar),
         Location,
         TileOmnimods
      ),

   Result.

-spec handle_characters
   (
      list(rst_character:type()),
      non_neg_integer(),
      btl_map:type(),
      ordsets:ordset(btl_location:type()),
      non_neg_integer(),
      orddict:orddict(non_neg_integer(), btl_character:type()),
      list(ataxic:basic())
   )
   ->
   {
      orddict:orddict(non_neg_integer(), btl_character:type()),
      list(ataxic:basic())
   }.
handle_characters
(
   [],
   _PlayerIX,
   _Map,
   _UsedLocations,
   _NextCharIX,
   Characters,
   AtaxicUpdates
) ->
   {Characters, AtaxicUpdates};
handle_characters
(
   [RosterCharacter|NextRosterCharacters],
   PlayerIX,
   Map,
   UsedLocations,
   NextCharIX,
   Characters,
   AtaxicUpdates
) ->
   NewCharacter =
      create_character(PlayerIX, RosterCharacter, Map, UsedLocations),

   NewCharacters = orddict:store(NextCharIX, NewCharacter, Characters),

   NewUpdate =
      ataxic:apply_function
      (
         orddict,
         store,
         [
            ataxic:constant(NextCharIX),
            ataxic:constant(NewCharacter),
            ataxic:current_value()
         ]
      ),

   handle_characters
   (
      NextRosterCharacters,
      PlayerIX,
      Map,
      [btl_character:get_location(NewCharacter)|UsedLocations],
      (NextCharIX + 1),
      NewCharacters,
      [NewUpdate|AtaxicUpdates]
   ).

-spec add_player
   (
      shr_player:id(),
      btl_battle:type()
   )
   -> {btl_battle:type(), non_neg_integer(), ataxic:basic()}.
add_player (PlayerID, Battle) ->
   Players = btl_battle:get_players(Battle),

   PlayerIX = orddict:size(Players),
   NewPlayer = btl_player:new(PlayerIX, 0, PlayerID),

   NewPlayers = orddict:store(PlayerIX, NewPlayer, Players),
   S0Battle = btl_battle:set_players(NewPlayers, Battle),

   Update =
      ataxic:update_field
      (
         btl_battle:get_players_field(),
         ataxic:apply_function
         (
            orddict,
            store,
            [
               ataxic:constant(PlayerIX),
               ataxic:constant(NewPlayer),
               ataxic:current_value()
            ]
         )
      ),

   {S0Battle, PlayerIX, Update}.

-spec add_characters
   (
      list(rst_character:type()),
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

   OldWeaponIDs = btl_battle:get_used_portrait_ids(Battle),
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
   -> list(rst_character:type()).
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
      list(non_neg_integer()),
      btl_pending_battle:type()
   )
   -> {btl_pending_battle:type(), ataxic:basic()}.
add_to_pending_battle
(
   PlayerID,
   PlayerSumIX,
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
   {S0Battle, PlayerIX, BattleUpdate0} = add_player(PlayerID, Battle),
   {S1Battle, BattleUpdate1} =
      add_characters(NewCharacters, PlayerIX, S0Battle),

   S0PendingBattle = btl_pending_battle:set_battle(S1Battle, PendingBattle),
   S1PendingBattle =
      btl_pending_battle:set_free_slots(RemainingSlots, S0PendingBattle),
   S2PendingBattle =
      btl_pending_battle:set_player_ids
      (
         [PlayerID|btl_pending_battle:get_player_ids(S1PendingBattle)],
         S1PendingBattle
      ),
   S3PendingBattle =
      btl_pending_battle:set_player_summary_ixs
      (
         [PlayerIX|btl_pending_battle:get_player_summary_ixs(S1PendingBattle)],
         S2PendingBattle
      ),

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
      map_map:id(),
      list(non_neg_integer())
   )
   -> btl_pending_battle:type().
generate_pending_battle
(
   PlayerID,
   PlayerSumIX,
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
   TileInstances = map_map:get_tile_instances(Map),
   BattleMap =
      btl_map:from_instances_tuple
      (
         map_map:get_width(Map),
         map_map:get_height(Map),
         TileInstances
      ),

   Battle = btl_battle:new(BattleMap),

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
         SelectedRosterCharacterIXs,
         PendingBattle
      ),

   S0PendingBattle.

-spec repair_join_battle
   (
      shr_player:id(),
      non_neg_integer(),
      list(non_neg_integer()),
      btl_pending_battle:id(),
      btl_pending_battle:type()
   )
   -> {ok, btl_pending_battle:type()}.
repair_join_battle
(
   PlayerID,
   PlayerSumIX,
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
      list(non_neg_integer()),
      btl_pending_battle:id(),
      map_map:id()
   )
   -> {ok, btl_pending_battle:type()}.
repair_create_battle (PlayerID, PlayerSumIX, RosterCharIXs, PBattleID, MapID) ->
   NewPendingBattle =
      generate_pending_battle(PlayerID, PlayerSumIX, MapID, RosterCharIXs),

   ok =
      ataxia_client:update
      (
         pending_battle_db,
         ataxia_security:user_from_id(PlayerID),
         ataxic:update_value(ataxic:constant(NewPendingBattle)),
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
   -> ok.
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
      map_map:id(),
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
      0 -> repair_generate_battle(PendingBattleID, S0PendingBattle);
      _ -> ok
   end,

   ok.
