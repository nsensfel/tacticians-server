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
      characters :: array:array(rst_character:type())
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
      set_character/3,

      add_character/2,
      remove_character/2
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

-spec get_characters (type()) -> array:array(rst_character:type()).
get_characters (Roster) -> Roster#roster.characters.

-spec get_character (non_neg_integer(), type()) -> rst_character:type().
get_character (IX, Roster) -> array:get(IX, Roster#roster.characters).

-spec set_characters (array:array(rst_character:type()), type()) -> type().
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
      characters = array:set(IX, Character, Roster#roster.characters)
   }.

-spec add_character (rst_character:type(), type()) -> type().
add_character (Character, Roster) ->
   CurrentCharacters = Roster#roster.characters,
   CurrentSize = array:size(CurrentCharacters),

   Roster#roster
   {
      characters = array:set(CurrentSize, Character, CurrentCharacters)
   }.

-spec remove_character (non_neg_integer(), type()) -> type().
remove_character (IX, Roster) ->
   CurrentCharacters = Roster#roster.characters,
   CurrentSize = array:size(CurrentCharacters),
   NewSize = (CurrentSize - 1),
   LastCharacter = array:get(NewSize, CurrentCharacters),

   S0Characters = array:set(IX, LastCharacter, CurrentCharacters),
   S1Characters = array:resize(NewSize, S0Characters),

   Roster#roster
   {
      characters = S1Characters
   }.

-spec get_characters_field () -> non_neg_integer().
get_characters_field () -> #roster.characters.

-spec new (binary()) -> type().
new (Owner) ->
   #roster
   {
      owner = Owner,
      characters = array:from_list([rst_character:new(), rst_character:new()])
   }.
