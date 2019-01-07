-module(shr_player).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-type id() :: ataxia_id:type().

-record
(
   player,
   {
      username :: binary(),
      % {salt(crypto:strong_rand_bytes(128)), hash(sha384)}
      password :: {binary(), binary()},
      token :: binary(), % salt(crypto:strong_rand_bytes(512))
      email :: binary(),
      last_active :: integer(),
      maps :: orddict:orddict(non_neg_integer(), shr_map_summary:type()),
      campaigns :: orddict:orddict(non_neg_integer(), shr_battle_summary:type()),
      invasions :: orddict:orddict(non_neg_integer(), shr_battle_summary:type()),
      events :: orddict:orddict(non_neg_integer(), shr_battle_summary:type()),
      roster_id :: binary(),
      inventory_id :: binary()
   }
).

-opaque type() :: #player{}.

-export_type([type/0, id/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export
(
   [
      new/3
   ]
).

%%%% Accessors
-export
(
   [
      get_username/1,
      get_password/1,
      get_token/1,
      get_email/1,
      get_last_active/1,
      get_map_summaries/1,
      get_campaign_summaries/1,
      get_invasion_summaries/1,
      get_event_summaries/1,
      get_inventory_id/1,
      get_roster_id/1,

      set_username/2,
      set_password/2,
      new_token/1,
      set_email/2,
      refresh_active/1,
      set_map_summaries/2,
      set_campaign_summaries/2,
      set_invasion_summaries/2,
      set_event_summaries/2,
      set_inventory_id/2,
      set_roster_id/2
   ]
).

-export
(
   [
      get_username_field/0,
      get_password_field/0,
      get_token_field/0,
      get_email_field/0,
      get_last_active_field/0,
      get_map_summaries_field/0,
      get_campaign_summaries_field/0,
      get_invasion_summaries_field/0,
      get_event_summaries_field/0,
      get_inventory_id_field/0,
      get_roster_id_field/0
   ]
).

-export
(
   [
      password_is/2
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec secure_value (binary(), binary()) -> binary().
secure_value (Salt, Val) ->
   % TODO [SECURITY][LOW]: Maybe it would be a good idea to include the user's
   % IP in there as well. This would ensure that sessions alway use the same
   % server (and thus, the same caches), and make timed cache exploits easier to
   % prevent.
   SaltedVal = erlang:iolist_to_binary([Salt, Val]),
   HashedSaltedVal = crypto:hash(sha384, SaltedVal),

   HashedSaltedVal.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec new (binary(), binary(), binary()) -> type().
new (Username, Password, Email) ->
   EmptyBattleSlot = shr_battle_summary:none(),
   SixEmptyBattleSlots =
      orddict:from_list
      (
         [
            {0, EmptyBattleSlot},
            {1, EmptyBattleSlot},
            {2, EmptyBattleSlot},
            {3, EmptyBattleSlot},
            {4, EmptyBattleSlot},
            {5, EmptyBattleSlot}
         ]
      ),

   NineEmptyBattleSlots =
      orddict:store
      (
         6,
         EmptyBattleSlot,
         orddict:store
         (
            7,
            EmptyBattleSlot,
            orddict:store
            (
               8,
               EmptyBattleSlot,
               SixEmptyBattleSlots
            )
         )
      ),

   Result =
      #player
      {
         username = Username,
         password = {<<"">>, <<"">>},
         token = <<"">>,
         email = Email,
         last_active = 0,
         maps = orddict:new(),
         campaigns = SixEmptyBattleSlots,
         invasions = NineEmptyBattleSlots,
         events = SixEmptyBattleSlots,
         inventory_id = ataxia_id:null(),
         roster_id = ataxia_id:null()
      },

   S0Result = set_password(Password, Result),
   S1Result = new_token(S0Result),
   S2Result = refresh_active(S1Result),

   S2Result.

%%%% Accessors
-spec get_username (type()) -> binary().
get_username (Player) -> Player#player.username.

-spec get_password (type()) -> {binary(), binary()}.
get_password (Player) -> Player#player.password.

-spec get_token (type()) -> binary().
get_token (Player) -> Player#player.token.

-spec get_email (type()) -> binary().
get_email (Player) -> Player#player.email.

-spec get_last_active (type()) -> integer().
get_last_active (Player) -> Player#player.last_active.

-spec get_map_summaries
   (
      type()
   )
   -> orddict:orddict(non_neg_integer(), shr_map_summary:type()).
get_map_summaries (Player) -> Player#player.maps.

-spec get_campaign_summaries
   (
      type()
   )
   -> orddict:orddict(non_neg_integer(), shr_battle_summary:type()).
get_campaign_summaries (Player) -> Player#player.campaigns.

-spec get_invasion_summaries
   (
      type()
   )
   -> orddict:orddict(non_neg_integer(), shr_battle_summary:type()).
get_invasion_summaries (Player) -> Player#player.invasions.

-spec get_event_summaries
   (
      type()
   )
   -> orddict:orddict(non_neg_integer(), shr_battle_summary:type()).
get_event_summaries (Player) -> Player#player.events.

-spec get_roster_id (type()) -> ataxia_id:type().
get_roster_id (Player) -> Player#player.roster_id.

-spec get_inventory_id (type()) -> ataxia_id:type().
get_inventory_id (Player) -> Player#player.inventory_id.

-spec set_username (binary(), type()) -> type().
set_username (Val, Player) -> Player#player{ username = Val }.

-spec set_password (binary(), type()) -> type().
set_password (Val, Player) ->
   NewSalt = crypto:strong_rand_bytes(128),
   HashedSaltedVal = secure_value(NewSalt, Val),

   Player#player
   {
      password = {NewSalt, HashedSaltedVal}
   }.

-spec new_token (type()) -> type().
new_token (Player) ->
   Player#player
   {
      token = base64:encode(crypto:strong_rand_bytes(512))
   }.

-spec set_email (binary(), type()) -> type().
set_email (Val, Player) -> Player#player{ email = Val }.

-spec refresh_active (type()) -> type().
refresh_active (Player) ->
   Player#player
   {
      last_active = erlang:system_time(second)
   }.

-spec set_map_summaries
   (
      orddict:orddict(non_neg_integer(), shr_map_summary:type()),
      type()
   )
   -> type().
set_map_summaries (Maps, Player) -> Player#player{ maps = Maps }.

-spec set_campaign_summaries
   (
      orddict:orddict(non_neg_integer(), shr_battle_summary:type()),
      type()
   )
   -> type().
set_campaign_summaries (Campaigns, Player) ->
   Player#player
   {
      campaigns = Campaigns
   }.

-spec set_invasion_summaries
   (
      orddict:orddict(non_neg_integer(), shr_battle_summary:type()),
      type()
   )
   -> type().
set_invasion_summaries (Invasions, Player) ->
   Player#player
   {
      invasions = Invasions
   }.

-spec set_event_summaries
   (
      orddict:orddict(non_neg_integer(), shr_battle_summary:type()),
      type()
   )
   -> type().
set_event_summaries (Events, Player) ->
   Player#player
   {
      events = Events
   }.

-spec set_roster_id (binary(), type()) -> type().
set_roster_id (RosterID, Player) -> Player#player{ roster_id = RosterID }.

-spec set_inventory_id (binary(), type()) -> type().
set_inventory_id (InvID, Player) -> Player#player{ inventory_id = InvID }.

-spec get_username_field () -> non_neg_integer().
get_username_field () -> #player.username.

-spec get_password_field () -> non_neg_integer().
get_password_field () -> #player.password.

-spec get_token_field () -> non_neg_integer().
get_token_field () -> #player.token.

-spec get_email_field () -> non_neg_integer().
get_email_field () -> #player.email.

-spec get_last_active_field () -> non_neg_integer().
get_last_active_field () -> #player.last_active.

-spec get_map_summaries_field () -> non_neg_integer().
get_map_summaries_field () -> #player.maps.

-spec get_campaign_summaries_field () -> non_neg_integer().
get_campaign_summaries_field () -> #player.campaigns.

-spec get_invasion_summaries_field () -> non_neg_integer().
get_invasion_summaries_field () -> #player.invasions.

-spec get_event_summaries_field () -> non_neg_integer().
get_event_summaries_field () -> #player.events.

-spec get_roster_id_field () -> non_neg_integer().
get_roster_id_field () -> #player.roster_id.

-spec get_inventory_id_field () -> non_neg_integer().
get_inventory_id_field () -> #player.inventory_id.

-spec password_is (binary(), type()) -> boolean().
password_is (Val, Player) ->
   {Salt, HashedSaltedVal} = Player#player.password,
   HashedSaltedCandidate = secure_value(Salt, Val),

   (HashedSaltedCandidate == HashedSaltedVal).
