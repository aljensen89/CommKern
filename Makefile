PKG_ROOT    = .
PKG_VERSION = $(shell awk '/^Version:/{print $$2}' $(PKG_ROOT)/DESCRIPTION)
PKG_NAME    = $(shell awk '/^Package:/{print $$2}' $(PKG_ROOT)/DESCRIPTION)

CRAN = "https://cran.rstudio.com"

# General Package Dependencies
SRC       = $(wildcard $(PKG_ROOT)/src/*.cpp)
RFILES    = $(wildcard $(PKG_ROOT)/R/*.R)
TESTS     = $(wildcard $(PKG_ROOT)/tests/*.R)

# Targets
#
## Vignettes
# These are both targets for building and dependencies for the package tar.gz
# file
# VIGNETTES  = $(PKG_ROOT)/vignettes/

## Data targets
#DATATARGETS  = $(PKG_ROOT)/data/mtcars2.rda
#DATATARGETS += $(PKG_ROOT)/data/pefr.rda

################################################################################
# Recipes

.PHONY: all check install clean

all: $(PKG_NAME)_$(PKG_VERSION).tar.gz

$(PKG_NAME)_$(PKG_VERSION).tar.gz: .install_dev_deps.Rout .document.Rout $(VIGNETTES) $(TESTS)
	R CMD build --md5 $(build-options) $(PKG_ROOT)

.install_dev_deps.Rout : $(PKG_ROOT)/DESCRIPTION
	Rscript --vanilla --quiet -e "options(repo = c('$(CRAN)'))" \
		-e "if (!require(devtools)) {install.packages('devtools', repo = c('$(CRAN)'))}" \
		-e "options(warn = 2)" \
		-e "devtools::install_dev_deps()"
	touch $@

.document.Rout: $(SRC) $(RFILES) $(DATATARGETS) $(EXAMPLES) $(PKG_ROOT)/DESCRIPTION
	Rscript --vanilla --quiet -e "options(warn = 2)" \
		-e "devtools::document('$(PKG_ROOT)')"
	touch $@

check: $(PKG_NAME)_$(PKG_VERSION).tar.gz
	R CMD check $(PKG_NAME)_$(PKG_VERSION).tar.gz

check-as-cran: $(PKG_NAME)_$(PKG_VERSION).tar.gz
	R CMD check --as-cran $(PKG_NAME)_$(PKG_VERSION).tar.gz

install: $(PKG_NAME)_$(PKG_VERSION).tar.gz
	R CMD INSTALL $(PKG_NAME)_$(PKG_VERSION).tar.gz

uninstall :
	R --vanilla --quiet -e "try(remove.packages('$(PKG_NAME).data'), silent = TRUE)"

clean:
	$(RM) -f  $(PKG_NAME)_$(PKG_VERSION).tar.gz
	$(RM) -rf $(PKG_NAME).Rcheck
	$(RM) -f .document.Rout
	$(RM) -f .install_dev_deps.Rout
