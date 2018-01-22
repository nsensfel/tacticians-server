-module(weapon).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record
(
   weapon,
   {
      id,
      name,
      icon,
      type,
      pwr_min,
      pwr_max
   }
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Accessors
-export
(
   [
      get_id/1,
      get_name/1,
      get_icon/1,
      get_type/1,
      get_max_power/1,
      get_min_power/1
   ]
).

-export
(
   [
      get_category/1,
      get_ranges/1
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ranges_of_type (short_bow) ->    {2, 4};
ranges_of_type (long_bow) ->     {2, 6};
ranges_of_type (crossbow) ->     {2, 4};
ranges_of_type (arbalest) ->     {2, 4};
ranges_of_type (sword) ->        {1, 1};
ranges_of_type (claymore) ->     {1, 2};
ranges_of_type (mace) ->         {1, 1};
ranges_of_type (war_hammer) ->   {1, 2};
ranges_of_type (dagger) ->       {1, 1};
ranges_of_type (spear) ->        {1, 2}.

category_of_type (short_bow) ->  physical;
category_of_type (long_bow) ->   physical;
category_of_type (crossbow) ->   physical;
category_of_type (arbalest) ->   physical;
category_of_type (sword) ->      physical;
category_of_type (claymore) ->   physical;
category_of_type (mace) ->       physical;
category_of_type (war_hammer) -> physical;
category_of_type (dagger) ->     physical;
category_of_type (spear) ->      physical.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Accessors
get_id (Wp) -> Wp#weapon.id.
get_name (Wp) -> Wp#weapon.name.
get_icon (Wp) -> Wp#weapon.icon.
get_type (Wp) -> Wp#weapon.type.
get_max_power (Wp) -> Wp#weapon.pwr_max.
get_min_power (Wp) -> Wp#weapon.pwr_min.

get_ranges (Wp) -> ranges_of_type(Wp#weapon.type).
get_category (Wp) -> category_of_type(Wp#weapon.type).
