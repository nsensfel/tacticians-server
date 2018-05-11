-module(turn_result).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
-record
(
   switched_weapon,
   {
      character_instance_ix :: character_instance:id()
   }
).

-record
(
   moved,
   {
      character_instance_ix :: character_instance:id(),
      path :: list(direction:enum()),
      new_location :: location:type()
   }
).

-record
(
   attacked,
   {
      attacker_ix :: character_instance:id(),
      defender_ix :: character_instance:id(),
      sequence :: list(attack:type())
   }
).

-opaque type() :: (#switched_weapon{} | #moved{} | #attacked{}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export_type([type/0]).

-export
(
   [
      new_character_switched_weapons/1,
      new_character_moved/3,
      new_character_attacked/3
   ]
).

-export
(
   [
      encode/1
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec new_character_switched_weapons (character_instance:id()) -> type().
new_character_switched_weapons (CharacterInstanceIX) ->
   #switched_weapon { character_instance_ix = CharacterInstanceIX }.

-spec new_character_moved
   (
      character_instance:id(),
      list(direction:enum()),
      location:type()
   )
   -> type().
new_character_moved (CharacterInstanceIX, Path, NewLocation) ->
   #moved
   {
      character_instance_ix = CharacterInstanceIX,
      path = Path,
      new_location = NewLocation
   }.

-spec new_character_attacked
   (
      character_instance:id(),
      character_instance:id(),
      list(attack:type())
   )
   -> type().
new_character_attacked (AttackerIX, DefenderIX, AttackSequence) ->
   #attacked
   {
      attacker_ix = AttackerIX,
      defender_ix = DefenderIX,
      sequence = AttackSequence
   }.

-spec encode (type()) -> {list(any())}.
encode (TurnResult) when is_record(TurnResult, switched_weapon) ->
   CharacterInstanceIX = TurnResult#switched_weapon.character_instance_ix,

   {
      [
         {<<"t">>, <<"swp">>},
         {<<"ix">>, CharacterInstanceIX}
      ]
   };
encode (TurnResult) when is_record(TurnResult, moved) ->
   CharacterInstanceIX = TurnResult#moved.character_instance_ix,
   Path = TurnResult#moved.path,
   NewLocation = TurnResult#moved.new_location,

   EncodedPath = lists:map(fun direction:encode/1, Path),
   EncodedNewLocation = location:encode(NewLocation),

   {
      [
         {<<"t">>, <<"mv">>},
         {<<"ix">>, CharacterInstanceIX},
         {<<"p">>, EncodedPath},
         {<<"nlc">>, EncodedNewLocation}
      ]
   };
encode (TurnResult) when is_record(TurnResult, attacked) ->
   AttackerIX = TurnResult#attacked.attacker_ix,
   DefenderIX = TurnResult#attacked.defender_ix,
   Sequence = TurnResult#attacked.sequence,

   EncodedSequence = lists:map(fun attack:encode/1, Sequence),

   {
      [
         {<<"t">>, <<"atk">>},
         {<<"aix">>, AttackerIX},
         {<<"dix">>, DefenderIX},
         {<<"seq">>, EncodedSequence}
      ]
   };
encode (Other) ->
   io:format("~n invalid encode param\"~p\"~n", [Other]),
   true = Other.
