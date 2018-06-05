-module(storage_access).

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
get_value ([]) -> not_found;
get_value ([Regval]) -> {ok, db_item:get_value(Regval)}.

read_transaction (DB, ID) ->
   get_value(mnesia:read(DB, ID)).

insert_transaction (DB, ID, Perm, Value) ->
   StoredItem = db_item:new(ID, Perm, Value),
   % FIXME: handle return value, mnesia:write -> (transaction abort | ok).
   % FIXME: is this an atomic OP? Is the lock freed afterwards?
   mnesia:write(DB, StoredItem, sticky_write),
   ok.

query_transaction (Query) ->
   DB = db_query:get_database(Query),
   ID = db_query:get_entry_id(Query),
   [Item] = mnesia:read(DB, ID),
   {ok, UpdatedItem} = db_query:apply_to(Query, Item),
   % FIXME: handle return value, mnesia:write -> (transaction abort | ok).
   % FIXME: is this an atomic OP? Is the lock freed afterwards?
   mnesia:write(DB, UpdatedItem, sticky_write),
   ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
read (DB, ID) ->
   mnesia:transaction(fun read_transaction/2, [DB, ID]).

insert (DB, ID, Perm, Value) ->
   mnesia:transaction(fun insert_transaction/4, [DB, ID, Perm, Value]).

query (Query) ->
   mnesia:transaction(fun query_transaction/1, [Query]).
