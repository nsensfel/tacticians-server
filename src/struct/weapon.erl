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
      range_type,
      range_mod,
      damage_type,
      damage_mod
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
      random_id/0
   ]
).

-export
(
   [
      from_id/1,
      get_ranges/1,
      get_damages/1,
      apply_to_attributes/2
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ranges_of_type (ranged, long) -> {2, 6};
ranges_of_type (ranged, short) -> {1, 4};
ranges_of_type (melee, long) -> {0, 2};
ranges_of_type (melee, short) -> {0, 1}.

damages_of_type (ranged, heavy) -> {10, 25};
damages_of_type (ranged, light) -> {5, 20};
damages_of_type (melee, heavy) -> {20, 35};
damages_of_type (melee, light) -> {15, 30}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Accessors
get_id (Wp) -> Wp#weapon.id.

get_ranges (Wp) ->
   ranges_of_type(Wp#weapon.range_type, Wp#weapon.range_mod).
get_damages (Wp) ->
   damages_of_type(Wp#weapon.range_type, Wp#weapon.damage_mod).

from_id (0) ->
   #weapon{
      id = 0,
      name = "None",
      range_type = melee,
      range_mod = short,
      damage_type = blunt,
      damage_mod = light
   };
from_id (1) ->
   #weapon{
      id = 1,
      name = "Dagger",
      range_type = melee,
      range_mod = short,
      damage_type = slash,
      damage_mod = light
   };
from_id (2) ->
   #weapon{
      id = 2,
      name = "Sword",
      range_type = melee,
      range_mod = short,
      damage_type = slash,
      damage_mod = heavy
   };
from_id (3) ->
   #weapon{
      id = 3,
      name = "Claymore",
      range_type = melee,
      range_mod = long,
      damage_type = slash,
      damage_mod = light
   };
from_id (4) ->
   #weapon{
      id = 4,
      name = "Bardiche",
      range_type = melee,
      range_mod = long,
      damage_type = slash,
      damage_mod = heavy
   };
from_id (5) ->
   #weapon{
      id = 5,
      name = "Stiletto",
      range_type = melee,
      range_mod = short,
      damage_type = pierce,
      damage_mod = light
   };
from_id (6) ->
   #weapon{
      id = 6,
      name = "Pickaxe",
      range_type = melee,
      range_mod = short,
      damage_type = pierce,
      damage_mod = heavy
   };
from_id (7) ->
   #weapon{
      id = 7,
      name = "Rapier",
      range_type = melee,
      range_mod = long,
      damage_type = pierce,
      damage_mod = light
   };
from_id (8) ->
   #weapon{
      id = 8,
      name = "Pike",
      range_type = melee,
      range_mod = long,
      damage_type = pierce,
      damage_mod = heavy
   };
from_id (9) ->
   #weapon{
      id = 9,
      name = "Club",
      range_type = melee,
      range_mod = short,
      damage_type = blunt,
      damage_mod = light
   };
from_id (10) ->
   #weapon{
      id = 10,
      name = "Mace",
      range_type = melee,
      range_mod = short,
      damage_type = blunt,
      damage_mod = heavy
   };
from_id (11) ->
   #weapon{
      id = 11,
      name = "Staff",
      range_type = melee,
      range_mod = long,
      damage_type = blunt,
      damage_mod = light
   };
from_id (12) ->
   #weapon{
      id = 12,
      name = "War Hammer",
      range_type = melee,
      range_mod = long,
      damage_type = blunt,
      damage_mod = heavy
   };
from_id (13) ->
   #weapon{
      id = 13,
      name = "Short Bow (Broadhead)",
      range_type = ranged,
      range_mod = short,
      damage_type = slash,
      damage_mod = light
   };
from_id (14) ->
   #weapon{
      id = 14,
      name = "Short Bow (Blunt)",
      range_type = ranged,
      range_mod = short,
      damage_type = blunt,
      damage_mod = light
   };
from_id (15) ->
   #weapon{
      id = 15,
      name = "Short Bow (Bodkin Point)",
      range_type = ranged,
      range_mod = short,
      damage_type = pierce,
      damage_mod = light
   };
from_id (16) ->
   #weapon{
      id = 16,
      name = "Long Bow (Broadhead)",
      range_type = ranged,
      range_mod = long,
      damage_type = slash,
      damage_mod = light
   };
from_id (17) ->
   #weapon{
      id = 17,
      name = "Long Bow (Blunt)",
      range_type = ranged,
      range_mod = long,
      damage_type = blunt,
      damage_mod = light
   };
from_id (18) ->
   #weapon{
      id = 18,
      name = "Long Bow (Bodkin Point)",
      range_type = ranged,
      range_mod = long,
      damage_type = pierce,
      damage_mod = light
   };
from_id (19) ->
   #weapon{
      id = 19,
      name = "Crossbow (Broadhead)",
      range_type = ranged,
      range_mod = short,
      damage_type = slash,
      damage_mod = heavy
   };
from_id (20) ->
   #weapon{
      id = 20,
      name = "Crossbow (Blunt)",
      range_type = ranged,
      range_mod = short,
      damage_type = blunt,
      damage_mod = heavy
   };
from_id (21) ->
   #weapon{
      id = 21,
      name = "Crossbow (Bodkin Point)",
      range_type = ranged,
      range_mod = short,
      damage_type = pierce,
      damage_mod = heavy
   };
from_id (22) ->
   #weapon{
      id = 22,
      name = "Arbalest (Broadhead)",
      range_type = ranged,
      range_mod = long,
      damage_type = slash,
      damage_mod = heavy
   };
from_id (23) ->
   #weapon{
      id = 23,
      name = "Arbalest (Blunt)",
      range_type = ranged,
      range_mod = long,
      damage_type = blunt,
      damage_mod = heavy
   };
from_id (24) ->
   #weapon{
      id = 24,
      name = "Arbalest (Bodkin Point)",
      range_type = ranged,
      range_mod = long,
      damage_type = pierce,
      damage_mod = heavy
   }.

random_id () ->
   roll:between(0, 24).

apply_to_attributes (Attributes, Weapon) ->
   Dexterity = attributes:get_dexterity(Attributes),
   Speed = attributes:get_dexterity(Attributes),
   RangeModifier = Weapon#weapon.range_mod,
   DamageModifier = Weapon#weapon.damage_mod,
   WithRangeModifier =
      case RangeModifier of
         long ->
            attributes:set_dexterity(max(0, (Dexterity - 20)), Attributes);
         _ -> Attributes
      end,
   case DamageModifier of
      heavy ->
         attributes:set_speed(max(0, (Speed - 20)), WithRangeModifier);
      _ -> WithRangeModifier
   end.

