-module(btl_character_turn_update).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record
(
   type,
   {
      battle :: btl_battle:type(),
      character_ix :: non_neg_integer(),
      reversed_battle_updates :: list(ataxic:basic()),
      timeline :: list(any())
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
      new/2,

      get_battle/1,

      get_character_ix/1,
      get_battle_update/1,
      get_timeline/1,

      set_battle/2,
      ataxia_set_battle/3,

      add_to_timeline/2
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec new (btl_battle:type(), non_neg_integer()) -> type().
new (Battle, CharacterIX) ->
   #type
   {
      battle = Battle,
      character_ix = CharacterIX,
      reversed_battle_updates = [],
      timeline = []
   }.

-spec get_battle (type()) -> btl_battle:type().
get_battle (Data) -> Data#type.battle.

-spec get_character_ix (type()) -> non_neg_integer().
get_character_ix (Data) -> Data#type.character_ix.

-spec set_battle (btl_battle:type(), type()) -> type().
set_battle (Battle, Data) -> Data#type { battle = Battle }.

-spec ataxia_set_battle
   (
      btl_battle:type(),
      ataxic:basic(),
      type()
   )
   -> type().
ataxia_set_battle (Battle, BattleUpdate, Data) ->
   Data#type
   {
      battle = Battle,
      reversed_battle_updates = [BattleUpdate|Data#type.reversed_battle_updates]
   }.

-spec add_to_timeline (btl_turn_result:type(), type()) -> type().
add_to_timeline (Item, Data) ->
   Data#type
   {
      timeline = [btl_turn_result:encode(Item)|Data#type.timeline]
   }.

-spec get_timeline (type()) -> list(any()).
get_timeline (Data) -> Data#type.timeline.

-spec get_battle_update (type()) -> ataxic:basic().
get_battle_update (Data) ->
   ataxic:sequence(lists:reverse(Data#type.reversed_battle_updates)).
