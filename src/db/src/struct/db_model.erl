-module(db_model).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record
(
   db_model,
   {
      name :: atom(),
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
      new/3,
      start/1
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec new (atom(), string(), list(node())) -> type().
new (DBName, StorageFile, Neighbors) ->
   #db_model
   {
      name = DBName,
      store_file = StorageFile,
      neighbors = Neighbors
   }.

start (Model) ->
   DBName = Model#db_model.name,
   StorageFile = Model#db_model.store_file,
   Neighbors = Model#db_model.neighbors,

   ok = application:set_env(mnesia, dir, StorageFile),
   case mnesia:create_schema([node()|Neighbors]) of
      {error, {Name, {already_exists, Name}}} -> ok;
      ok -> ok
   end,
   ok = mnesia:start(),
   mnesia:create_table
   (
      DBName,
      [
         {record_name, db_item:get_record_name()},
         {attributes, db_item:get_record_info()},
         {disc_copies, [node()|Neighbors]},
         {disc_only_copies, []},
         {ram_copies, []},
         {type, ordered_set},
         {local_content, false}
      ]
   ).
