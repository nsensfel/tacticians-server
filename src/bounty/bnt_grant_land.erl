-module(bnt_grant_land).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([attempt/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec attempt (shr_player:id()) -> shr_map:type().
attempt (OwnerID) ->
   Map = shr_map:default(OwnerID),

   {ok, MapID} =
      ataxia_client:add
      (
         map_db,
         ataxia_security:allow_only(ataxia_security:any()),
         ataxia_security:allow_only(ataxia_security:user_from_id(OwnerID)),
         Map
      ),

   MapSummary = shr_map_summary:new(MapID, <<"Untitled Map">>),

   PlayerUpdateQueryOp =
      ataxic:update_value
      (
         ataxic:update_field
         (
            shr_player:get_map_summaries_field(),
            ataxic:apply_function
            (
               orddict,
               store,
               [
                  ataxic:apply_function
                  (
                     orddict,
                     size,
                     [ataxic:current_value()]
                  ),
                  ataxic:constant(MapSummary),
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
         PlayerUpdateQueryOp,
         OwnerID
      ),

   Map.
