################################################################################
## USER CONFIGURATION OPTIONS ##################################################
################################################################################
## Main Directories
SRC_DIR ?= ${CURDIR}/src
CONFIG_DIR ?= ${CURDIR}/conf

## Optional Directories
BIN_DIR ?= ${CURDIR}/ebin
INCLUDE_DIR ?= ${CURDIR}/include
WWW_DIR ?= ${CURDIR}/www
LOG_DIR ?= ${CURDIR}/log

## Binaries
YAWS ?= yaws
YAWS_OPTS ?=

ERLC ?= erlc
ERLC_OPTS ?=

ERL ?= erl
ERL_OPTS ?=

DIALYZER ?= dialyzer
DIALYZER_OPTS ?=

M4 ?= m4
M4_OPTS ?=

## Filenames
DIALYZER_PLT_FILE ?= tacticians-server.plt

################################################################################
## MAKEFILE MAGIC ##############################################################
################################################################################
## General
OPTIONAL_DIRS = $(BIN_DIR) $(INCLUDE_DIR) $(WWW_DIR) $(LOG_DIR)

## Preprocessor
PREPROCESSOR_CONFIG_FILES = $(shell find $(CONFIG_DIR) -name "*.m4.conf")
PREPROCESSABLE_FILES = $(shell find ${CURDIR} -name "*.m4")
PREPROCESSED_FILES = $(patsubst %.m4,%,$(PREPROCESSABLE_FILES))

## Erlang
ERL_SRC_FILES = $(shell find $(SRC_DIR) -name "*.erl")
ERL_BIN_FILES = $(patsubst %.erl,$(BIN_DIR)/%.beam,$(notdir $(ERL_SRC_FILES)))

## Yaws
REQUIRED_HEADERS = $(INCLUDE_DIR)/yaws_api.hrl

################################################################################
## SANITY CHECKS ###############################################################
################################################################################

################################################################################
## TARGET RULES ################################################################
################################################################################
all: build

debug: debug_run

build: $(OPTIONAL_DIRS) $(REQUIRED_HEADERS) $(PREPROCESSED_FILES) $(ERL_BIN_FILES)

run: yaws_run

clean:
	# Preprocessor
	rm -rf $(PREPROCESSED_FILES)
	# Erlang
	rm -rf $(BIN_DIR)/*

reset:
	# Preprocessor
	rm -rf $(PREPROCESSED_FILES)
	rm -rf $(OPTIONAL_DIRS)

################################################################################
## INTERNAL RULES ##############################################################
################################################################################
$(PREPROCESSED_FILES): %: $(PREPROCESSOR_CONFIG_FILES) %.m4
	$(M4) -P $^ > $@

$(OPTIONAL_DIRS): %:
	mkdir -p $@

.SECONDEXPANSION:
$(ERL_BIN_FILES): $(BIN_DIR)/%.beam: $$(shell find $(SRC_DIR) -name "%.erl")
	$(ERLC) $(ERLC_OPTS) -o $(BIN_DIR) $<
