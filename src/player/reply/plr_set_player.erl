-module(plr_set_player).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([generate/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec generate (shr_player:type()) -> {list(any())}.
generate (Player) ->
   MapList =
      lists:map
      (
         fun shr_map_summary:encode/1,
         array:to_list(shr_player:get_map_summaries(Player))
      ),

   CampaignList =
      lists:map
      (
         fun shr_battle_summary:encode/1,
         array:to_list(shr_player:get_campaign_summaries(Player))
      ),

   InvasionList =
      lists:map
      (
         fun shr_battle_summary:encode/1,
         array:to_list(shr_player:get_invasion_summaries(Player))
      ),

   EventList =
      lists:map
      (
         fun shr_battle_summary:encode/1,
         array:to_list(shr_player:get_event_summaries(Player))
      ),

   {
      [
         {<<"msg">>, <<"set_plr">>},
         {<<"id">>, shr_player:get_id(Player)},
         {<<"nme">>, shr_player:get_username(Player)},
         {<<"maps">>, MapList},
         {<<"cmps">>, CampaignList},
         {<<"invs">>, InvasionList},
         {<<"evts">>, EventList},
         {<<"rtid">>, shr_player:get_roster_id(Player)},
         {<<"ivid">>, shr_player:get_inventory_id(Player)}
      ]
   }.
