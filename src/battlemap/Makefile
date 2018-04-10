################################################################################
## CONFIG ######################################################################
################################################################################
YAWS_CONF ?= $(CONF_DIR)/yaws.conf
YAWS_API_HEADER ?= /my/src/yaws/include/yaws_api.hrl

DIALYZER_PLT_FILE ?= tacticians-server.plt

## Main Directories
SRC_DIR ?= src
CONF_DIR ?= conf
#### Optional Dirs
BIN_DIR ?= ebin
INCLUDE_DIR ?= include

## Binaries
YAWS ?= yaws
ERLC ?= erlc
ERLC_OPTS ?=
DIALYZER ?= dialyzer

################################################################################
## MAKEFILE MAGIC ##############################################################
################################################################################
OPTIONAL_DIRS = $(BIN_DIR) $(INCLUDE_DIR)
REQUIRED_HEADERS = $(INCLUDE_DIR)/yaws_api.hrl

################################################################################
## SANITY CHECKS ###############################################################
################################################################################
YAWS_API_HEADER ?= /my/src/yaws/include/yaws_api.hrl
DIALYZER_PLT_FILE ?= tacticians-server.plt

## Main Directories
SRC_DIR ?= src
CONF_DIR ?= conf

################################################################################
## INCLUDES ####################################################################
################################################################################
main_target: all

include ${CURDIR}/mk/debug.mk
include ${CURDIR}/mk/erlang.mk
include ${CURDIR}/mk/preprocessor.mk
include ${CURDIR}/mk/yaws.mk

################################################################################
## TARGET RULES ################################################################
################################################################################
all: build

debug: debug_run

build: $(PREPROCESSOR_RESULT) $(ERLANG_RESULT)

run: yaws_run

clean:
	rm -rf $(PREPROCESSOR_RESULT) $(ERLANG_RESULT)

################################################################################
## INTERNAL RULES ##############################################################
################################################################################
$(OPTIONAL_DIRS): %:
	mkdir -p $@
