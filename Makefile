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

## Local only?
#ERL_NAME_VS_SNAME ?= -name
ERL_NAME_VS_SNAME ?= -sname

## Binaries
ERLC ?= erlc
ERLC_OPTS ?=

ERL ?= erl
ERL_OPTS ?= -connect_all false -pa $(BIN_DIR)

YAWS ?= yaws
YAWS_OPTS ?= $(ERL_NAME_VS_SNAME) query_node -erlarg "$(ERL_OPTS)"

DIALYZER ?= dialyzer
DIALYZER_OPTS ?=

M4 ?= m4
M4_OPTS ?=

## Filenames
DIALYZER_PLT_FILE ?= tacticians-server.plt

YAWS_CONFIG_FILE ?= $(CONFIG_DIR)/yaws.conf


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

## Binaries
YAWS_EXEC = $(YAWS) $(YAWS_OPTS)
ERLC_EXEC = $(ERLC) $(ERLC_OPTS)
ERL_EXEC = $(ERL) $(ERL_OPTS)
DIALYZER_EXEC = $(DIALYZER) $(DIALYZER_OPTS)
M4_EXEC = $(M4) $(M4_OPTS)

################################################################################
## SANITY CHECKS ###############################################################
################################################################################


################################################################################
## PREPROCESSOR-VISIBLE MAKEFILE VARIABLES #####################################
################################################################################

MAKEFILE_TO_M4 = \
	--define=__MAKEFILE_BIN_DIR=$(BIN_DIR) \
	--define=__MAKEFILE_LOG_DIR=$(LOG_DIR) \
	--define=__MAKEFILE_WWW_DIR=$(WWW_DIR) \
	--define=__MAKEFILE_INCLUDE_DIR=$(INCLUDE_DIR)
################################################################################
## TARGET RULES ################################################################
################################################################################
all: build

debug: debug_run

build: $(OPTIONAL_DIRS) $(REQUIRED_HEADERS) \
	$(PREPROCESSED_FILES) $(ERL_BIN_FILES)

run_db_node: build
	$(ERL_EXEC) $(ERL_NAME_VS_SNAME) db_node -run db_node start

run_query_node: build $(YAWS_CONFIG_FILE)
	$(YAWS_EXEC) --conf $(YAWS_CONFIG_FILE)

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
debug_rebuild:
	$(MAKE) clean
	$(MAKE) ERLC_OPTS="$(ERLC_OPTS) +debug_info"

ifeq ($(wildcard $(DIALYZER_PLT_FILE)),)
debug_run:
	$(DIALYZER_EXEC) --build_plt --apps erts kernel stdlib jiffy mnesia \
		--output_plt $(DIALYZER_PLT_FILE)
	$(MAKE) debug_rebuild
	$(DIALYZER_EXEC) --add_to_plt --plt $(DIALYZER_PLT_FILE) -r $(BIN_DIR)
else
debug_run:
	$(MAKE) debug_rebuild
	$(DIALYZER_EXEC) --check_plt --plt $(DIALYZER_PLT_FILE)
	$(DIALYZER_EXEC) --get_warnings $(ERL_SRC_FILES) \
		--src --plt $(DIALYZER_PLT_FILE)
endif

$(PREPROCESSED_FILES): %: $(PREPROCESSOR_CONFIG_FILES) %.m4
	$(M4_EXEC) -P $(MAKEFILE_TO_M4) $^> $@

$(OPTIONAL_DIRS): %:
	mkdir -p $@

.SECONDEXPANSION:
$(ERL_BIN_FILES): $(BIN_DIR)/%.beam: $$(shell find $(SRC_DIR) -name "%.erl")
	$(ERLC_EXEC) -o $(BIN_DIR) $<
