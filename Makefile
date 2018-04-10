################################################################################
## CONFIG ######################################################################
################################################################################
MODULES ?= battlemap
MODULES_DIR ?= ${CURDIR}/src

YAWS_CONFIG_TEMPLATE ?= ${CURDIR}/conf/yaws.conf.m4
YAWS_API_HEADER ?= /my/src/yaws/include/yaws_api.hrl
################################################################################
## MAKEFILE MAGIC ##############################################################
################################################################################
MODULES_SRC = $(addprefix $(MODULES_DIR)/,$(MODULES))

################################################################################
## SANITY CHECKS ###############################################################
################################################################################
MISSING_MODULES_DIR = \
	$(filter-out $(wildcard $(MODULES_SRC)),$(MODULES_SRC))

ifneq ($(MISSING_MODULES_DIR),)
$(error "The following modules are missing: $(MISSING_MODULES_DIR)")
endif

################################################################################
## TARGET RULES ################################################################
################################################################################
export

all:
	for module in $(MODULES_SRC) ; do \
		$(MAKE) -C $$module all; \
	done

debug:
	for module in $(MODULES_SRC) ; do \
		$(MAKE) -C $$module debug ; \
	done

build:
	for module in $(MODULES_SRC) ; do \
		$(MAKE) -C $$module build ; \
	done

run:
	for module in $(MODULES_SRC) ; do \
		$(MAKE) -C $$module run; \
	done

clean:
	for module in $(MODULES_SRC) ; do \
		$(MAKE) -C $$module clean; \
	done

################################################################################
## INTERNAL RULES ##############################################################
################################################################################
