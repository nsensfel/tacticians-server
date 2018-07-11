-module(db_access).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export
(
   [
      read/2,
      insert/4,
      query/1
   ]
).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec get_value (list(shr_db_item:type())) -> ({'ok', any()} | 'not_found').
get_value ([]) -> not_found;
get_value ([Regval]) -> {ok, shr_db_item:get_value(Regval)}.

-spec read_transaction (atom(), any()) -> ({'ok', any()} | 'not_found').
read_transaction (DB, ID) ->
   get_value(mnesia:read(DB, ID)).

-spec insert_transaction
   (
      atom(),
      any(),
      shr_db_user:permission(),
      any()
   )
   -> 'ok'.
insert_transaction (DB, ID, Perm, Value) ->
   StoredItem = shr_db_item:new(ID, Perm, Value),
   % FIXME: handle return value, mnesia:write -> (transaction abort | ok).
   % FIXME: is this an atomic OP? Is the lock freed afterwards?
   mnesia:write(DB, StoredItem, sticky_write),
   ok.

-spec query_transaction (shr_db_query:type()) -> 'ok'.
query_transaction (Query) ->
   DB = shr_db_query:get_database(Query),
   ID = shr_db_query:get_entry_id(Query),
   [Item] = mnesia:read(DB, ID),
   {ok, UpdatedItem} = shr_db_query:apply_to(Query, Item),
   % FIXME: handle return value, mnesia:write -> (transaction abort | ok).
   % FIXME: is this an atomic OP? Is the lock freed afterwards?
   mnesia:write(DB, UpdatedItem, sticky_write),
   ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec read (atom(), any())
   -> ({'aborted', any()} | {'atomic', ({'ok', any()} | 'not_found')}).
read (DB, ID) ->
   mnesia:transaction(fun read_transaction/2, [DB, ID]).

-spec insert (atom(), any(), shr_db_user:permission(), any())
   -> ({'aborted', any()} | {'atomic', 'ok'}).
insert (DB, ID, Perm, Value) ->
   mnesia:transaction(fun insert_transaction/4, [DB, ID, Perm, Value]).

-spec query (shr_db_query:type()) -> ({'aborted', any()} | {'atomic', 'ok'}).
query (Query) ->
   mnesia:transaction(fun query_transaction/1, [Query]).
