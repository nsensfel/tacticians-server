################################################################################
## CONFIG ######################################################################
################################################################################
YAWS_CONF ?= $(CONF_DIR)/yaws.conf
YAWS_API_HEADER ?= /my/src/yaws/include/yaws_api.hrl

YAWS ?= yaws

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
yaws_run: all $(UNUSED_WWW_DIR)
	$(YAWS) --conf $(YAWS_CONF)

################################################################################
## INTERNAL RULES ##############################################################
################################################################################
$(INCLUDE_DIR)/yaws_api.hrl: $(YAWS_API_HEADER) $(INCLUDE_DIR)
	cp $< $@
