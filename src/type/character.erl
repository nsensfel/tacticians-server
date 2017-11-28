-module(character).
-record
(
   character,
   {
      id,
      name,
      icon,
      portrait,
      health,
      mov_pts,
      atk_rg
   }
).
-export
(
   [
      get_id/1,
      get_name/1,
      get_icon/1,
      get_portrait/1,
      get_max_health/1,
      get_movement_points/1,
      get_attack_range/1
   ]
).

get_id (Char) -> Char#character.id.
get_name (Char) -> Char#character.name.
get_icon (Char) -> Char#character.icon.
get_portrait (Char) -> Char#character.portrait.
get_max_health (Char) -> Char#character.health.
get_movement_points (Char) -> Char#character.mov_pts.
get_attack_range (Char) -> Char#character.atk_rg.