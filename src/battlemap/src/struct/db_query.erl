-module(db_query).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
-record
(
   set_field,
   {
      field :: non_neg_integer(),
      value :: any()
   }
).

-record
(
   add_to_field,
   {
      field :: non_neg_integer(),
      values :: list(any())
   }
).

-record
(
   update_indexed,
   {
      field :: non_neg_integer(),
      ix :: non_neg_integer(),
      update :: list(type())
   }
).

-opaque type() :: (#update_indexed{} | #set_field{} | #add_to_field{}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export_type([type/0]).

-export
(
   [
      set_field/2,
      add_to_field/2,
      update_indexed/3
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec set_field (non_neg_integer(), any()) -> type().
set_field (Field, Value) ->
   #set_field { field = Field, value = Value }.

-spec add_to_field (non_neg_integer(), list(any())) -> type().
add_to_field (Field, Values) ->
   #add_to_field { field = Field, values = Values }.

-spec update_indexed
   (
      non_neg_integer(),
      non_neg_integer(),
      list(type())
   )
   -> type().
update_indexed (Field, IX, Updates) ->
   #update_indexed { field = Field, ix = IX, update = Updates}.
