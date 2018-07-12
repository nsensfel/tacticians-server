-module(db_model).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record
(
   db_model,
   {
      store_file :: string(),
      neighbors :: list(node())
   }
).

-type type() :: #db_model{}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export_type([type/0]).

-export
(
   [
      new/2,
      add_db/2,
      start/1
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec new (string(), list(node())) -> type().
new (StorageFile, Neighbors) ->
   #db_model
   {
      store_file = StorageFile,
      neighbors = Neighbors
   }.

-spec start(type()) -> 'ok'.
start (Model) ->
   StorageFile = Model#db_model.store_file,
   Neighbors = Model#db_model.neighbors,

   ok = application:set_env(mnesia, dir, StorageFile),

   case mnesia:create_schema([node()|Neighbors]) of
      {error, {Name, {already_exists, Name}}} -> ok;
      ok -> ok
   end,

   ok = mnesia:start(),

   ok.

-spec add_db (atom(), type()) -> 'ok'.
add_db (DBName, Model) ->
   Neighbors = Model#db_model.neighbors,

   mnesia:create_table
   (
      DBName,
      [
         {record_name, shr_db_item:get_record_name()},
         {attributes, shr_db_item:get_record_info()},
         {disc_copies, [node()|Neighbors]},
         {disc_only_copies, []},
         {ram_copies, []},
         {type, ordered_set},
         {local_content, false}
      ]
   ),

   ok.
