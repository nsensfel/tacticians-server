-module(battlemap_node).
-export([start/1]).

start(_YawsParams) ->
   {ok, Pid} = gen_server:start(timed_caches_manager, [], []),
   gen_server:cast(Pid, {add, battlemaps_db, none}),
%%   timed_caches_manager:add_cache([], battlemaps_db, none),
   receive
      after 5000 ->
         [] = ets:lookup(battlemaps_db, <<"00">>)
   end.
