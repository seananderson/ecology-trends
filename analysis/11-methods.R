library(tidyverse)
source("analysis/plot-panels.R")
source("analysis/extract-functions.R")

# --------
# Sean:
d <- read.csv("data/methods-models.csv", strip.white = TRUE, stringsAsFactors = FALSE)
terms <- unique(d$gram)
d$gram <- tolower(d$gram)
# out <- get_ngram_dat(terms)
# saveRDS(out, file = "data/generated/method-grams.rds")
out_sean <- readRDS("data/generated/method-grams.rds")
d <- full_join(d, out, by = "gram") %>%
  filter(!is.na(total))

plot_panels(d, "figs/methods-panels-1.pdf", palette = "Dark2")

# --------
# Brent:
d <- read.csv("data/conservation-terms.csv", strip.white = TRUE, stringsAsFactors = FALSE)
terms <- unique(d$gram)
d$gram <- tolower(d$gram)
# out <- get_ngram_dat(terms)
# saveRDS(out, file = "data/generated/conservation-grams.rds")
out <- readRDS("data/generated/conservation-grams.rds")
d <- full_join(d, out, by = "gram") %>%
  filter(!is.na(total))
plot_panels(d, "figs/conservation-panels-1.pdf", width = 10, height = 8)

# --------
# Paul:
d <- read.csv("data/paul_human_impacts.csv", strip.white = TRUE, stringsAsFactors = FALSE)
terms <- unique(d$gram)
d$gram <- tolower(d$gram)
d$show <- "yes"
# out <- get_ngram_dat(terms)
# saveRDS(out, file = "data/generated/human-impacts-grams.rds")
out <- readRDS("data/generated/human-impacts-grams.rds")
out <- filter(out, gram %in% d$gram)
d <- full_join(d, out, by = "gram") %>%
  filter(!is.na(total))
plot_panels(d, "figs/human-impacts-panels-1.pdf", width = 10, height = 8)

# --------
# Becky:
d <- read.csv("data/scale_panels_1_nohyphens_18dec.csv",
  strip.white = TRUE, stringsAsFactors = FALSE)
terms <- unique(d$gram)
d$gram <- tolower(d$gram)
# out <- get_ngram_dat(terms)
# saveRDS(out, file = "data/generated/becky-2017-12-14.rds")
out <- readRDS("data/generated/becky-2017-12-14.rds")
d <- full_join(d, out, by = "gram") %>%
  filter(!is.na(total))
plot_panels(d, "figs/scale-panels-1.pdf", width = 10, height = 12)
