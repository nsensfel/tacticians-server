## Directories
SRC_DIR ?= src
BIN_DIR ?= ebin
CONF_DIR ?= conf
INCLUDE_DIR ?= include
UNUSED_WWW_DIR ?= www

YAWS_CONF ?= $(CONF_DIR)/yaws.conf
YAWS_API_HEADER ?= /my/src/yaws/include/yaws_api.hrl

DIALYZER_PLT_FILE ?= tacticians-server.plt

## Binaries
YAWS ?= yaws
ERLC ?= erlc
ERLC_OPTS ?=
DIALYZER ?= dialyzer

################################################################################
REQUIRED_HEADERS = $(INCLUDE_DIR)/yaws_api.hrl

SRC_FILES = $(wildcard $(SRC_DIR)/*.erl)
MODULES = $(patsubst %.erl,%,$(SRC_FILES))
SUB_DIRS = $(filter-out $(MODULES),$(sort $(dir $(wildcard $(SRC_DIR)/*/))))
BIN_FILES = $(patsubst $(SRC_DIR)/%.erl,$(BIN_DIR)/%.beam,$(SRC_FILES))

export
################################################################################
all:
	for subdir in $(SUB_DIRS) ; do \
		echo "Building dir $$subdir" ; \
		$(MAKE) build SRC_DIR=$$subdir || exit 1;\
	done

debug: $(DIALYZER_PLT_FILE)
	$(MAKE) build_debug
	$(DIALYZER) --check_plt --plt $(DIALYZER_PLT_FILE)
	$(DIALYZER) --get_warnings $(SRC_DIR)/*.erl $(SRC_DIR)/*/*.erl \
		--src --plt $(DIALYZER_PLT_FILE) -Wunderspecs

build_debug:
	$(MAKE) clean
	$(MAKE) ERLC_OPTS=+debug_info

build: $(BIN_DIR) $(REQUIRED_HEADERS) $(BIN_FILES)

run: all $(UNUSED_WWW_DIR)
	$(YAWS) --conf $(YAWS_CONF)

clean:
	rm -rf $(BIN_DIR)/*

$(DIALYZER_PLT_FILE):
	$(DIALYZER) --build_plt --apps erts kernel stdlib jiffy --output_plt $@
	$(MAKE) build_debug
	$(DIALYZER) --add_to_plt --plt $@ -r $(BIN_DIR)

$(INCLUDE_DIR)/yaws_api.hrl: $(YAWS_API_HEADER) $(INCLUDE_DIR)
	cp $< $@

$(BIN_DIR):
	mkdir -p $@

$(UNUSED_WWW_DIR):
	mkdir -p $@

$(INCLUDE_DIR):
	mkdir -p $@

.SECONDEXPANSION:
$(BIN_FILES): $(BIN_DIR)/%.beam : $(SRC_DIR)/%.erl $$(wildcard $$(SRC_DIR)/%/.)
	$(ERLC) $(ERLC_OPTS) -o $(BIN_DIR) $<
