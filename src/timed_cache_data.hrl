%% TODO: add types.
-record(battlemap, {id, width, height, content, instances}).
-record(battlemap_instance, {id, chars, curr_player, players, rem_chars, last_turn}).
-record(character, {id, name, icon, portrait, health, mov_pts, atk_rg}).
-record(character_turn, {id, path, target}).
-record(player, {id, battlemaps, characters}).

%% Not stored in its own timed cache.
-record(character_instance, {x, y, health, team}).
