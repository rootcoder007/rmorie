# SPDX-License-Identifier: AGPL-3.0-or-later
#
# rmorie -- terminal-native development workflow.
#
# Use from your isolated user-space checkout (e.g. ~/workspace/rmorie/
# or /Volumes/VSR/rootcoderfiles/rmorie-staging/), NOT from any
# system R library directory. This keeps you out of /usr/lib/R/library
# and ~/R/x86_64-pc-linux-gnu-library territory where Linux policies
# fight you for control.
#
# Pairs with renv -- run `make renv-init` ONCE to create the
# project-local R library at ./renv/library/. After that every
# devtools::test() / devtools::check() / etc. resolves dependencies
# from the local renv library, not from your system or user library.

R       ?= R
RSCRIPT ?= Rscript

.DEFAULT_GOAL := help

.PHONY: help all renv-init renv-snapshot renv-restore document build check \
        test test-fast coverage lint goodpractice pkgcheck install clean \
        docker docker-run docs

help:
	@echo "rmorie Makefile -- terminal-native dev workflow"
	@echo ""
	@echo "  Setup (one-time):"
	@echo "    make renv-init       -- create ./renv/ project-local R library"
	@echo "    make renv-restore    -- restore deps from renv.lock"
	@echo ""
	@echo "  Development loop:"
	@echo "    make document        -- regen NAMESPACE + man/*.Rd"
	@echo "    make test            -- devtools::test()  (full suite)"
	@echo "    make test-fast       -- skip slow tests"
	@echo "    make check           -- R CMD check"
	@echo "    make build           -- build source tarball"
	@echo ""
	@echo "  Quality checks (mirrors CI):"
	@echo "    make coverage        -- covr report"
	@echo "    make lint            -- lintr::lint_package()"
	@echo "    make goodpractice    -- rolls up covr + cyclocomp + lintr + rcmdcheck"
	@echo "    make pkgcheck        -- rOpenSci pkgcheck"
	@echo ""
	@echo "  Container:"
	@echo "    make docker          -- build local image"
	@echo "    make docker-run      -- smoke-load library(rmorie) in container"
	@echo ""
	@echo "  Other:"
	@echo "    make install         -- install into current R library (NOT renv)"
	@echo "    make docs            -- pkgdown::build_site() (rmorie.org if/when public)"
	@echo "    make clean           -- remove build artifacts"

all: document build check

# --- renv ---

renv-init:
	$(RSCRIPT) -e 'if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv"); renv::init(bare = TRUE)'

renv-snapshot:
	$(RSCRIPT) -e 'renv::snapshot()'

renv-restore:
	$(RSCRIPT) -e 'renv::restore()'

# --- dev loop ---

document:
	$(RSCRIPT) -e 'devtools::document(".", quiet = FALSE)'

build:
	$(RSCRIPT) -e 'devtools::build(".", path = "../")'

check:
	$(RSCRIPT) -e 'devtools::check(".", args = c("--as-cran", "--no-manual"))'

test:
	$(RSCRIPT) -e 'devtools::test()'

test-fast:
	$(RSCRIPT) -e 'Sys.setenv(NOT_CRAN = "false"); devtools::test()'

# --- quality (CI parity) ---

coverage:
	$(RSCRIPT) -e 'cov <- covr::package_coverage(type = "tests", quiet = FALSE); cat(sprintf("Coverage: %.2f%%\n", covr::percent_coverage(cov)))'

lint:
	$(RSCRIPT) -e 'lintr::lint_package()'

goodpractice:
	$(RSCRIPT) -e 'g <- goodpractice::gp("."); print(g)'

pkgcheck:
	$(RSCRIPT) -e 'cks <- pkgcheck::pkgcheck("."); md <- pkgcheck::checks_to_markdown(cks); writeLines(md, "pkgcheck.md"); cat(md)'

# --- install / docker / docs ---

install:
	$(R) CMD INSTALL --no-test-load .

docker:
	docker build -t rmorie:dev .

docker-run:
	docker run --rm rmorie:dev

docs:
	$(RSCRIPT) -e 'pkgdown::build_site(".", devel = FALSE)'

clean:
	rm -rf ../rmorie_*.tar.gz rmorie.Rcheck docs _site coverage.xml pkgcheck.md
	find . -name '*.o' -delete
	find . -name '*.so' -delete
	find . -name '*.dll' -delete
