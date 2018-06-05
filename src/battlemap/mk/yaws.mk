################################################################################
## CONFIG ######################################################################
################################################################################
YAWS_CONF ?= ${CURDIR}/yaws.conf
YAWS_API_HEADER ?= /my/src/yaws/include/yaws_api.hrl

YAWS ?= yaws
YAWS_OPTS ?= -name battlemap_node -erlarg "-connect_all false"

################################################################################
## MAKEFILE MAGIC ##############################################################
################################################################################

################################################################################
## SANITY CHECKS ###############################################################
################################################################################
YAWS_API_HEADER ?= /my/src/yaws/include/yaws_api.hrl

################################################################################
## TARGET RULES ################################################################
################################################################################
yaws_run: build $(WWW_DIR) $(LOG_DIR)
	$(YAWS) --conf $(YAWS_CONF) $(YAWS_OPTS)

################################################################################
## INTERNAL RULES ##############################################################
################################################################################
$(INCLUDE_DIR)/yaws_api.hrl: $(YAWS_API_HEADER) $(INCLUDE_DIR)
	cp $< $@
