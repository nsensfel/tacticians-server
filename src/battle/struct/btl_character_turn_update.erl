-module(btl_character_turn_update).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record
(
   type,
   {
      data :: btl_character_turn_data:type(),
      timeline :: list(any()),
      db :: list(sh_db_query:op())
   }
).

-opaque type() :: #type{}.

-export_type([type/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export
(
   [
      new/1,

      get_data/1,
      get_timeline/1,
      get_db/1,

      set_data/2,
      add_to_timeline/3,
      add_to_db/2
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec new (btl_character_turn_data:type()) -> type().
new (Data) ->
   #type
   {
      data = Data,
      timeline = [],
      db = []
   }.

-spec get_data (type()) -> btl_character_turn_data:type().
get_data (Update) -> Update#type.data.

-spec get_timeline (type()) -> list(any()).
get_timeline (Update) -> Update#type.timeline.

-spec get_db (type()) -> list(sh_db_query:op()).
get_db (Update) -> Update#type.db.

-spec set_data (btl_character_turn_data:type(), type()) -> type().
set_data (Data, Update) ->
   Update#type{ data = Data}.

-spec add_to_timeline
   (
      btl_turn_result:type(),
      sh_db_query:op(),
      type()
   ) -> type().
add_to_timeline (Item, DBUpdate, Update) ->
   add_to_db
   (
      DBUpdate,
      Update#type
      {
         timeline = [btl_turn_result:encode(Item)|Update#type.timeline]
      }
   ).

-spec add_to_db (sh_db_query:op(), type()) -> type().
add_to_db (Item, Update) ->
   Update#type{ db = [Item|Update#type.db] }.
