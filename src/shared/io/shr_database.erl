-module(shr_database).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export
(
   [
      insert/4,
      fetch/2,
      commit/1
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec get_db_node () -> node().
get_db_node () -> list_to_atom("db_node@" ++ net_adm:localhost()).

-spec do_remote_operation (atom(), list(any())) ->
   (
      {'badrpc', any()}
      | {'aborted', any()}
      | {'atomic', ({'ok', any()} | 'ok' | 'not_found')}
   ).
do_remote_operation (Op, Params) ->
   rpc:call(get_db_node(), db_access, Op, Params).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec insert (atom(), any(), shr_db_user:permission(), any()) -> 'ok'.
insert (DB, ObjectID, Permission, Value) ->
   {atomic, _} = do_remote_operation(insert, [DB, ObjectID, Permission, Value]),
   io:format
   (
      "~nshr_database:insert(~p) -> ok.~n",
      [{DB, ObjectID, Permission, Value}]
   ),

   ok.

-spec fetch (atom(), any()) -> ({'ok', any()} | 'not_found').
fetch (DB, ObjectID) ->
   {atomic, Reply} = do_remote_operation(read, [DB, ObjectID]),
   io:format("~nshr_database:fetch(~p) -> ~p.~n", [{DB, ObjectID}, Reply]),
   Reply.

-spec commit (shr_db_query:type()) -> 'ok'.
commit (Query) ->
   {atomic, ok} = do_remote_operation(query, [Query]),
   io:format("~nshr_database:commit(~p) -> ok.~n", [Query]),
   ok.
