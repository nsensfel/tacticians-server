################################################################################
## CONFIG ######################################################################
################################################################################
CONFIG_FILE ?= ${CURDIR}/module.conf
YAWS_CONFIG_TEMPLATE ?=

################################################################################
## MAKEFILE MAGIC ##############################################################
################################################################################
PREPROCESSOR_FILES = $(shell find ${CURDIR} -name "*.m4")
PREPROCESSED_FILES = $(patsubst %.m4,%,$(PREPROCESSOR_FILES))

MAKEFILE_TO_M4 = \
	--define=__MAKEFILE_MODULE_NAME=$(MODULE_NAME) \
	--define=__MAKEFILE_MODULE_PORT=$(MODULE_PORT) \
	--define=__MAKEFILE_BIN_DIR=$(BIN_DIR) \
	--define=__MAKEFILE_INCLUDE_DIR=$(INCLUDE_DIR) \
	--define=__MAKEFILE_LOG_DIR=$(LOG_DIR) \
	--define=__MAKEFILE_WWW_DIR=$(WWW_DIR)

################################################################################
## SANITY CHECKS ###############################################################
################################################################################
ifeq ($(wildcard $(CONFIG_FILE)),)
$(error "Missing CONFIG_FILE ($(CONFIG_FILE)).")
endif

ifeq ($(wildcard $(YAWS_CONFIG_TEMPLATE)),)
$(error "Missing YAWS_CONFIG_TEMPLATE ($(YAWS_CONFIG_TEMPLATE)).")
endif

################################################################################
## TARGET RULES ################################################################
################################################################################
PREPROCESSOR_RESULT = $(PREPROCESSED_FILES) yaws.conf

################################################################################
## INTERNAL RULES ##############################################################
################################################################################
$(PREPROCESSED_FILES): %: $(CONFIG_FILE) %.m4
	m4 -P $^ > $@

yaws.conf: $(CONFIG_FILE) $(YAWS_CONFIG_TEMPLATE)
	m4 -P $(MAKEFILE_TO_M4) $^ > $@
