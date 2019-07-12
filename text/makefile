TEXT = ecograms
REFS = refs

all: $(TEXT).pdf

%.pdf: %.Rmd $(REFS).bib article.tex ../data/bad-amnat.txt ../figs/decades-and-booms-viridis.pdf ../figs/blanks-viridis5.pdf ../figs/ecology-panels3.pdf ../figs/methods-models-genetics2.pdf ../figs/conservation-human-impacts.pdf ../figs/social-science-panels3.pdf ../data/generated/total1.rds ../data/generated/top-blank.rda ../data/generated/methods-models-genetics-out.csv ../data/taxa-specific-journal-classifications.csv nature.csl ../data/generated/pnas_exluded_pub_ids.rds ../figs/booms-decline.pdf ../figs/blanks-extras-2018-08-09.pdf
	Rscript -e "source('compile.R')"
continuous:
	while true; do make --silent; sleep 3; done
