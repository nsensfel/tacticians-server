-module(character_turn_update).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record
(
   type,
   {
      data :: character_turn_data:type(),
      timeline :: list(any()),
      db :: list(any())
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
      add_to_timeline/2,
      add_to_db/2
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec new (data:type()) -> type().
new (Data) ->
   #type
   {
      data = Data,
      timeline = [],
      db = []
   }.

-spec get_data (type()) -> character_turn_data:type().
get_data (Update) -> Update#type.data.

-spec get_timeline (type()) -> list(any()).
get_timeline (Update) -> Update#type.timeline.

-spec get_db (type()) -> list(any()).
get_db (Update) -> Update#type.db.

-spec set_data (character_turn_data:type(), type()) -> type().
set_data (Data, Update) ->
   Update#type{ data = Data}.

-spec add_to_timeline (any(), type()) -> type().
add_to_timeline (Item, Update) ->
   Update#type{ timeline = [Item|Update#type.timeline] }.

-spec add_to_db (any(), type()) -> type().
add_to_db (Item, Update) ->
   Update#type{ db = [Item|Update#type.db] }.
