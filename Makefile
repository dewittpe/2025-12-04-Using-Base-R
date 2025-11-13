SHELL := /bin/bash
RCMDBATCHVANILLA := R CMD BATCH --vanilla
RVANILLA := R --vanilla --quiet
RSCRIPTVANILLA := Rscript --vanilla --quiet

N := 10 20 50 100 200 500 1000 2000 5000 10000 20000 50000 100000 200000 500000 1000000
ITR :=  $(shell seq 1 100)
METHODS := via_base_matrix via_tidyverse via_data.table via_data.table_threads1 via_data.table_threads2 via_data.table_threads4 via_stats_reshape via_reduce_merge
BENCHMARKS := $(foreach m,$(METHODS),$(foreach i,$(ITR),$(foreach n,$(N),benchmarks/$(m)/$(n)/$(i).dput)))

define METHOD_RULE
benchmarks/$(1)/%.dput: benchmarks.R R/common.R R/$(1).R
	$$(RSCRIPTVANILLA) $$< $(1)/$$*
endef

CRAN = 'https://cran.rstudio.com'

.INTERMEDIATE: slides.Rmd
.PHONY: all clean

all: .pkgs slides.html

.pkgs:
	R --quiet --vanilla -e "options(repos = $(CRAN))"\
		-e "if (!('devtools' %in% rownames(installed.packages()))) {install.packages('devtools')}"\
		-e "devtools::install_cran('knitr')"\
		-e "devtools::install_cran('rmarkdown')"
		-e "devtools::install_cran('data.table')"
		-e "devtools::install_cran('tidyverse')"
	touch .pkgs

%.Rout: %.R
	$(RCMDBATCHVANILLA) $< $@

benchmark_summaries.png: benchmark_summaries.R $(BENCHMARKS)
	$(RSCRIPTVANILLA) $<

slides.html : slides.Rmd style.css benchmark_summaries.png
	$(RVANILLA) -e "rmarkdown::render('$<')"

$(foreach m,$(METHODS),$(eval $(call METHOD_RULE,$(m))))


clean:
	$(RM) .pkgs
	$(RM) *.html
	$(RM) *.png
