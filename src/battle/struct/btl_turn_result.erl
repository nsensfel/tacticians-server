-module(btl_turn_result).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record
(
   condition,
   {
      module :: atom(),
      params :: any()
   }
).

-record
(
   switched_weapon,
   {
      character_ix :: non_neg_integer()
   }
).

-record
(
   moved,
   {
      character_ix :: non_neg_integer(),
      path :: list(shr_direction:enum()),
      new_location :: shr_location:type()
   }
).

-record
(
   hit,
   {
      attacker_ix :: non_neg_integer(),
      defender_ix :: non_neg_integer(),
      category :: btl_attack:category(),
      precision :: btl_attack:precision(),
      is_critical :: boolean(),
      is_parry :: boolean(),
      damage :: non_neg_integer(),
      attacker_luck :: integer(),
      defender_luck :: integer()
   }
).

-record
(
   targetted,
   {
      attacker_ix :: non_neg_integer(),
      defender_ix :: non_neg_integer()
   }
).


-record
(
   player_won,
   {
      player_ix :: non_neg_integer()
   }
).

-record
(
   player_lost,
   {
      player_ix :: non_neg_integer()
   }
).

-record
(
   player_turn_started,
   {
      player_ix :: non_neg_integer()
   }
).

-opaque type() :: (
   #switched_weapon{}
   | #moved{}
   | #hit{}
   | #targetted{}
   | #player_won{}
   | #player_lost{}
   | #player_turn_started{}
   | #condition{}
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export_type([type/0]).

-export
(
   [
      new_condition/2,
      new_player_won/1,
      new_player_lost/1,
      new_player_turn_started/1,
      new_character_switched_weapons/1,
      new_character_moved/3,
      new_character_hit/9,
      new_character_targetted/2
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
-spec new_condition (atom(), any()) -> type().
new_condition (Module, Params) ->
   #condition{ module = Module, params = Params }.

-spec new_player_won (non_neg_integer()) -> type().
new_player_won (PlayerIX) ->
   #player_won { player_ix = PlayerIX }.

-spec new_player_lost (non_neg_integer()) -> type().
new_player_lost (PlayerIX) ->
   #player_lost { player_ix = PlayerIX }.

-spec new_player_turn_started (non_neg_integer()) -> type().
new_player_turn_started (PlayerIX) ->
   #player_turn_started { player_ix = PlayerIX }.

-spec new_character_switched_weapons (non_neg_integer()) -> type().
new_character_switched_weapons (CharacterIX) ->
   #switched_weapon { character_ix = CharacterIX }.

-spec new_character_moved
   (
      non_neg_integer(),
      list(shr_direction:enum()),
      shr_location:type()
   )
   -> type().
new_character_moved (CharacterIX, Path, NewLocation) ->
   #moved
   {
      character_ix = CharacterIX,
      path = Path,
      new_location = NewLocation
   }.

-spec new_character_targetted (non_neg_integer(), non_neg_integer()) -> type().
new_character_targetted (AttackerIX, DefenderIX) ->
   #targetted
   {
      attacker_ix = AttackerIX,
      defender_ix = DefenderIX
   }.

-spec new_character_hit
   (
      non_neg_integer(),
      non_neg_integer(),
      btl_attack:category(),
      btl_attack:precision(),
      boolean(),
      boolean(),
      non_neg_integer(),
      integer(),
      integer()
   )
   -> type().
new_character_hit
(
   AttackerIX,
   DefenderIX,
   Category,
   Precision,
   IsCritical,
   IsParry,
   Damage,
   AttackerLuck,
   DefenderLuck
) ->
   #hit
   {
      attacker_ix = AttackerIX,
      defender_ix = DefenderIX,
      category = Category,
      precision = Precision,
      is_critical = IsCritical,
      is_parry = IsParry,
      damage = Damage,
      attacker_luck = AttackerLuck,
      defender_luck = DefenderLuck
   }.

-spec encode (type()) -> {list(any())}.
encode (TurnResult) when is_record(TurnResult, switched_weapon) ->
   CharacterIX = TurnResult#switched_weapon.character_ix,

   {
      [
         {<<"t">>, <<"swp">>},
         {<<"ix">>, CharacterIX}
      ]
   };
encode (TurnResult) when is_record(TurnResult, moved) ->
   CharacterIX = TurnResult#moved.character_ix,
   Path = TurnResult#moved.path,
   NewLocation = TurnResult#moved.new_location,

   EncodedPath = lists:map(fun shr_direction:encode/1, Path),
   EncodedNewLocation = shr_location:encode(NewLocation),

   {
      [
         {<<"t">>, <<"mv">>},
         {<<"ix">>, CharacterIX},
         {<<"p">>, EncodedPath},
         {<<"nlc">>, EncodedNewLocation}
      ]
   };
encode (TurnResult) when is_record(TurnResult, hit) ->
   AttackerIX = TurnResult#hit.attacker_ix,
   DefenderIX = TurnResult#hit.defender_ix,
   Category = TurnResult#hit.category,
   Precision = TurnResult#hit.precision,
   IsCritical = TurnResult#hit.is_critical,
   IsParry = TurnResult#hit.is_parry,
   Damage = TurnResult#hit.damage,
   AttackerLuck = TurnResult#hit.attacker_luck,
   DefenderLuck = TurnResult#hit.defender_luck,

   {
      [
         {<<"t">>, <<"atk">>},
         {<<"aix">>, AttackerIX},
         {<<"dix">>, DefenderIX},
         {<<"ord">>, btl_attack:encode_category(Category)},
         {<<"pre">>, btl_attack:encode_precision(Precision)},
         {<<"cri">>, IsCritical},
         {<<"par">>, IsParry},
         {<<"dmg">>, Damage},
         {<<"alk">>, AttackerLuck},
         {<<"dlk">>, DefenderLuck}
      ]
   };
encode (TurnResult) when is_record(TurnResult, targetted) ->
   AttackerIX = TurnResult#targetted.attacker_ix,
   DefenderIX = TurnResult#targetted.defender_ix,

   {
      [
         {<<"t">>, <<"tar">>},
         {<<"aix">>, AttackerIX},
         {<<"dix">>, DefenderIX}
      ]
   };
encode (TurnResult) when is_record(TurnResult, player_won) ->
   PlayerIX = TurnResult#player_won.player_ix,

   {
      [
         {<<"t">>, <<"pwo">>},
         {<<"ix">>, PlayerIX}
      ]
   };
encode (TurnResult) when is_record(TurnResult, player_lost) ->
   PlayerIX = TurnResult#player_lost.player_ix,

   {
      [
         {<<"t">>, <<"plo">>},
         {<<"ix">>, PlayerIX}
      ]
   };
encode (TurnResult) when is_record(TurnResult, player_turn_started) ->
   PlayerIX = TurnResult#player_turn_started.player_ix,

   {
      [
         {<<"t">>, <<"pts">>},
         {<<"ix">>, PlayerIX}
      ]
   };
encode (TurnResult) when is_record(TurnResult, condition) ->
   {ModuleID, EncodedParams} =
      erlang:apply
      (
         TurnResult#condition.module,
         encode_turn_result,
         [
            TurnResult#condition.params
         ]
      ),

   {
      [
         {<<"t">>, <<"con">>},
         {<<"m">>, ModuleID},
         {<<"p">>, EncodedParams}
      ]
   };
encode (Other) ->
   error(io_lib:format("~n invalid encode param\"~p\"~n", [Other])).
