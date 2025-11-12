SHELL := /bin/bash
RCMDBATCHVANILLA := R CMD BATCH --vanilla
RVANILLA := R --vanilla --quiet
RSCRIPTVANILLA := Rscript --vanilla --quiet

N := 10 100 1000 10000 100000
ITR :=  $(shell seq 1 100)
METHODS := via_base_matrix via_tidyverse via_data.table via_stats_reshape via_reduce_merge
BENCHMARKS := $(foreach m,$(METHODS),$(foreach i,$(ITR),$(foreach n,$(N),$(m)/$(n)/$(i).dput)))

BENCHMARKPLOTS := benchmarks_n10.png benchmarks_n100.png benchmarks_n1000.png benchmarks_n10000.png benchmarks_n100000.png

CRAN = 'https://cran.rstudio.com'

.INTERMEDIATE: slides.Rmd
.PHONY: all clean

all : .pkgs slides.html $(BENCHMARKS)

.pkgs :
	R --quiet --vanilla -e "options(repos = $(CRAN))"\
		-e "if (!('devtools' %in% rownames(installed.packages()))) {install.packages('devtools')}"\
		-e "devtools::install_cran('knitr')"\
		-e "devtools::install_cran('rmarkdown')"
		-e "devtools::install_cran('data.table')"
		-e "devtools::install_cran('tidyverse')"
	touch .pkgs

%.Rout: %.R
	$(RCMDBATCHVANILLA) $< $@

%.dput: benchmarks.R methods.R
	$(RSCRIPTVANILLA) $< $*

#benchmarks_n%.png: benchmarks.R methods.R
#	$(RSCRIPTVANILLA) $< $*

slides.html : slides.Rmd style.css #$(BENCHMARKPLOTS)
	$(RVANILLA) -e "rmarkdown::render('$<')"

clean:
	$(RM) .pkgs
	$(RM) *.html
	$(RM) *.png
