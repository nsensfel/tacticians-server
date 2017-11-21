## Directories
SRC_DIR ?= src
BIN_DIR ?= ebin
CONF_DIR ?= conf

YAWS_CONF = $(CONF_DIR)/yaws.conf

## Binaries
YAWS ?= yaws
ERLC ?= erlc

################################################################################
SRC_FILES = $(wildcard $(SRC_DIR)/*.erl)
MODULES = $(patsubst $(SRC_DIR)/%.erl,%,$(SRC_FILES))
BIN_FILES = $(patsubst $(SRC_DIR)/%.erl,$(BIN_DIR)/%.beam,$(SRC_FILES))
################################################################################
build: $(BIN_DIR) $(BIN_FILES)

run: $(BIN_FILES)
	$(YAWS) --conf $(YAWS_CONF)

clean:
	rm -rf $(BIN_DIR)/*

.SECONDEXPANSION:
$(BIN_FILES): $(BIN_DIR)/%.beam : $(SRC_DIR)/%.erl $$(wildcard $$(SRC_DIR)/%/.)
	$(ERLC) -o $(BIN_DIR) $<

$(BIN_DIR):
	mkdir -p $(BIN_DIR)
