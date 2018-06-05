################################################################################
## CONFIG ######################################################################
################################################################################
MODULE_NAME ?= $ $(shell basename ${CURDIR})
MODULE_PORT ?= 8002

DIALYZER_PLT_FILE ?= tacticians-server.plt

## Main Directories
SRC_DIR ?= ${CURDIR}/src
CONF_DIR ?= ${CURDIR}/conf

#### Optional Dirs
BIN_DIR ?= ${CURDIR}/ebin
INCLUDE_DIR ?= ${CURDIR}/include

## Binaries
ERLC ?= erlc
ERLC_OPTS ?=
ERL ?= erl
ERL_OPTS ?= \
	-connect_all false \
	-name db_node \
	-pa $(BIN_DIR) \
	-run db_node start

DIALYZER ?= dialyzer

################################################################################
## MAKEFILE MAGIC ##############################################################
################################################################################
OPTIONAL_DIRS = $(BIN_DIR) $(INCLUDE_DIR)

################################################################################
## SANITY CHECKS ###############################################################
################################################################################
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

################################################################################
## TARGET RULES ################################################################
################################################################################
all: build

debug: debug_run

build: $(REQUIRED_HEADERS) $(PREPROCESSOR_RESULT) $(ERLANG_RESULT)

run:
	$(ERL) $(ERL_OPTS)

clean:
	rm -rf $(PREPROCESSOR_RESULT) $(ERLANG_RESULT)

################################################################################
## INTERNAL RULES ##############################################################
################################################################################
$(OPTIONAL_DIRS): %:
	mkdir -p $@
