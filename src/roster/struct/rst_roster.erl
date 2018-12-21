-module(rst_roster).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-type id() :: binary().

-record
(
   roster,
   {
      owner :: binary(),
      characters :: orddict:orddict(non_neg_integer(), rst_character:type())
   }
).

-opaque type() :: #roster{}.

-export_type([type/0, id/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Accessors
-export
(
   [
      get_owner/1,
      get_characters/1,
      get_character/2,

      set_characters/2,
      set_character/3
   ]
).

-export
(
   [
      get_characters_field/0
   ]
).

-export
(
   [
      new/1
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Accessors
-spec get_owner (type()) -> binary().
get_owner (Roster) -> Roster#roster.owner.

-spec get_characters
   (
      type()
   )
   -> orddict:orddict(non_neg_integer(), rst_character:type()).
get_characters (Roster) -> Roster#roster.characters.

-spec get_character (non_neg_integer(), type()) -> rst_character:type().
get_character (IX, Roster) -> orddict:fetch(IX, Roster#roster.characters).

-spec set_characters
   (
      orddict:orddict(non_neg_integer(), rst_character:type()),
      type()
   )
   -> type().
set_characters (Characters, Roster) -> Roster#roster{ characters = Characters }.

-spec set_character
   (
      non_neg_integer(),
      rst_character:type(),
      type()
   )
   -> type().
set_character (IX, Character, Roster) ->
   Roster#roster
   {
      characters = orddict:store(IX, Character, Roster#roster.characters)
   }.

-spec get_characters_field () -> non_neg_integer().
get_characters_field () -> #roster.characters.

-spec new (binary()) -> type().
new (Owner) ->
   NewChar = rst_character:new(),
   #roster
   {
      owner = Owner,
      characters =
         orddict:from_list
         (
            [
               {0, NewChar},
               {1, NewChar},
               {2, NewChar},
               {3, NewChar},

               {4, NewChar},
               {5, NewChar},
               {6, NewChar},
               {7, NewChar}
            ]
         )
   }.
