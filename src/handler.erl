-module(handler).
-export([start/1]).

start(_YawsParams) ->
   {ok, Pid} = gen_server:start(timed_caches_manager, [], []),
   gen_server:cast(Pid, {add, battlemap_db, none}),
   gen_server:cast(Pid, {add, battlemap_instance_db, none}),
   gen_server:cast(Pid, {add, character_db, none}),
   gen_server:cast(Pid, {add, character_turn_db, none}),
   gen_server:cast(Pid, {add, player_data_db, none}),
   ok.
