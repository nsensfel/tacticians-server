-module(lgn_handler).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([start/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec ensure_player_exists (binary(), binary(), binary(), binary()) -> 'ok'.
ensure_player_exists (ID, Username, Password, Email) ->
   case shr_database:fetch(player_db, ID, admin) of
      {ok, _} -> ok;
      not_found ->
         shr_database:insert_at
         (
            player_db,
            ID,
            any,
            any,
            lgn_shim:generate_random_player(ID, Username, Password, Email)
         )
   end,

   case shr_database:fetch(login_db, Username, admin) of
      {ok, _} -> ok;
      not_found ->
         shr_database:insert_at
         (
            login_db,
            Username,
            any,
            any,
            ID
         )
   end,

   case shr_database:fetch(login_db, Email, admin) of
      {ok, _} -> ok;
      not_found ->
         shr_database:insert_at
         (
            login_db,
            Email,
            any,
            any,
            ID
         )
   end,

   ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec start (pid()) -> 'ok'.
start (TimedCachesManagerPid) ->
   ensure_player_exists
   (
      <<"0">>,
      <<"Player1">>,
      <<"Kalimer0">>,
      <<"P1@Tacticians.Online">>
   ),
   ensure_player_exists
   (
      <<"1">>,
      <<"Player2">>,
      <<"Kalimer1">>,
      <<"P2@Tacticians.Online">>
   ),
   shr_timed_caches_manager:new_cache(TimedCachesManagerPid, login_db, none),
   shr_timed_caches_manager:new_cache(TimedCachesManagerPid, player_db, none),
   ok.
