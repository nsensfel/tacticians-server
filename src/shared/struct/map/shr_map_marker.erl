-module(shr_map_marker).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-type name() :: binary().
-type type() :: {ataxia_security:permission(), list(shr_location:type())}.

-export_type([name/0, type/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export
(
   [
      can_access/2,
      get_locations/1
   ]
).

-export
(
   [
      decode/1,
      encode/1
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec can_access (ataxia_security:user(), type()) -> boolean().
can_access (User, {Permission, _Locations}) ->
   ataxia_security:can_access(User, Permission).

-spec get_locations (type()) -> boolean().
get_locations ({_Permission, Locations}) ->
   Locations.

-spec encode (type()) -> {list(any())}.
encode ({Permission, Locations}) ->
   {
      [
         {
            <<"p">>,
            ataxia_security:permission_to_json(fun (E) -> E end, Permission)
         },
         { <<"l">>, lists:map(fun shr_location:encode/1, Locations) }
      ]
   }.

-spec decode (map()) -> type().
decode (Map) ->
   EncodedPermission = maps:get("p", Map),
   EncodedLocations = maps:get("l", Map),

   Permission =
      ataxia_security:permission_from_json(fun (E) -> E end, EncodedPermission),
   Locations = lists:map(fun shr_location:decode/1, EncodedLocations),

   {
      Permission,
      Locations
   }.
