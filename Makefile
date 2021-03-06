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
DATA_DIR ?= /my/src/tacticians-data/

## Local only?
#ERL_NAME_VS_SNAME ?= -name
ERL_NAME_VS_SNAME ?= -sname

## Binaries
ERLC ?= erlc
ERLC_OPTS ?= -I $(INCLUDE_DIR)

ERL ?= erl
ERL_OPTS ?= -connect_all false -pa `find $(BIN_DIR) -type d`

YAWS ?= yaws
YAWS_OPTS ?= $(ERL_NAME_VS_SNAME) query_node -erlarg '$(ERL_OPTS)'

DIALYZER ?= dialyzer
DIALYZER_OPTS ?= -I $(INCLUDE_DIR)

M4 ?= m4
M4_OPTS ?=

## Filenames
DIALYZER_BASE_PLT_FILE ?= base.plt
DIALYZER_PLT_FILE ?= tacticians-server.plt

YAWS_CONFIG_FILE ?= $(CONFIG_DIR)/yaws.conf


################################################################################
## MAKEFILE MAGIC ##############################################################
################################################################################
## General
OPTIONAL_DIRS = $(BIN_DIR) $(INCLUDE_DIR) $(WWW_DIR) $(LOG_DIR)

## Preprocessor
PREPROCESSOR_CONFIG_FILES = $(shell find -L $(CONFIG_DIR) -name "*.m4.conf")
PREPROCESSABLE_FILES = $(shell find -L ${CURDIR} -name "*.m4")
PREPROCESSED_FILES = $(patsubst %.m4,%,$(PREPROCESSABLE_FILES))

## Erlang
PREPROCESSED_ERL_SRC_FILES = $(filter %.erl,$(PREPROCESSED_FILES))
CURRENT_ERL_SRC_FILES = $(shell find -L $(SRC_DIR) -name "*.erl")
ERL_SRC_FILES = \
	$(filter-out $(PREPROCESSED_ERL_SRC_FILES),$(CURRENT_ERL_SRC_FILES)) \
	$(PREPROCESSED_ERL_SRC_FILES)
ERL_BIN_FILES = $(patsubst $(SRC_DIR)%.erl,$(BIN_DIR)/%.beam,$(ERL_SRC_FILES))

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
ifeq ($(strip $(wildcard $(REQUIRED_HEADERS))),)
$(error "Could not find Yaws header (yaws_api.hrl). Install Yaws and copy the aforementioned file in the INCLUDE_DIR ($(INCLUDE_DIR)).")
endif

ifeq ($(strip $(wildcard $(DATA_DIR))),)
$(error "Could not find the game's data folder (currently set to $(DATA_DIR)). Download it and set the DATA_DIR variable to match its location.")
endif


################################################################################
## PREPROCESSOR-VISIBLE MAKEFILE VARIABLES #####################################
################################################################################
MAKEFILE_TO_M4 = \
	--define=__MAKEFILE_DATA_DIR=$(DATA_DIR) \
	--define=__MAKEFILE_BIN_DIR=$(BIN_DIR) \
	--define=__MAKEFILE_LOG_DIR=$(LOG_DIR) \
	--define=__MAKEFILE_WWW_DIR=$(WWW_DIR) \
	--define=__MAKEFILE_INCLUDE_DIR=$(INCLUDE_DIR) \
	--define=__CODE_STYLE=erlang

################################################################################
## TARGET RULES ################################################################
################################################################################
all: build
	@echo ""
	@echo ""
	@echo "#### Running the server"
	@echo "The server is split in two parts, which must both be run in parallel."
	@echo "1) 'make run_db_node' will start the DB managing part."
	@echo "2) 'make run_query_node' will start the query managing part."

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

$(DIALYZER_BASE_PLT_FILE):
	$(DIALYZER_EXEC) --build_plt --apps erts kernel stdlib crypto mnesia \
		--output_plt $@

debug_run: $(DIALYZER_BASE_PLT_FILE)
	$(MAKE) debug_rebuild
	cp $< $(DIALYZER_PLT_FILE)
	$(DIALYZER_EXEC) --add_to_plt --get_warnings --plt $(DIALYZER_PLT_FILE) -r $(BIN_DIR)
	$(DIALYZER_EXEC) --check_plt --plt $(DIALYZER_PLT_FILE)
	$(DIALYZER_EXEC) --get_warnings $(ERL_SRC_FILES) $(PREPROCESSED_ERL_SRC_FILES)\
		--src --plt $(DIALYZER_PLT_FILE)

$(PREPROCESSED_FILES): %: %.m4 .PHONY
	$(M4_EXEC) -P $(MAKEFILE_TO_M4) $(PREPROCESSOR_CONFIG_FILES) $< > $@

$(OPTIONAL_DIRS): %:
	mkdir -p $@

$(ERL_BIN_FILES): $(BIN_DIR)/%.beam: $(SRC_DIR)/%.erl
	mkdir -p $(dir $@)
	$(ERLC_EXEC) -o $(dir $@) $<

.PHONY:

