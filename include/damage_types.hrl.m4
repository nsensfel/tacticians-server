m4_include(__MAKEFILE_DATA_DIR/names.m4.conf)

-define(DAMAGE_TYPE_SLASH,    __SN_SLASH).
-define(DAMAGE_TYPE_PIERCE,   __SN_PIERCE).
-define(DAMAGE_TYPE_BLUNT,    __SN_BLUNT).

% Synonyms for blunt damage.
-define(DAMAGE_TYPE_BLUDGEONING, ?DAMAGE_TYPE_BLUNT).
-define(DAMAGE_TYPE_IMPACT,      ?DAMAGE_TYPE_BLUNT).

% Synonyms for pierce damage.
-define(DAMAGE_TYPE_PIERCING, ?DAMAGE_TYPE_PIERCE).
-define(DAMAGE_TYPE_PUNCTURE, ?DAMAGE_TYPE_PIERCE).

% Synonyms for slash damage.
-define(DAMAGE_TYPE_SLASHING, ?DAMAGE_TYPE_SLASH).
