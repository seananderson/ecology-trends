library(tidyverse)
source("analysis/plot-panels.R")
source("analysis/extract-functions.R")
source("analysis/pretty-panels.R")

pal_func <- function(n) {
  pal <- viridisLite::plasma(n, begin = 0.01, end = 0.84, direction = 1)
  gsub("FF$", "", pal)
  # RColorBrewer::brewer.pal(n, "Dark2")
  # rev(RColorBrewer::brewer.pal(n, "Spectral")
}

# --------
# Sean:
library(dplyr)
d <- read.csv("data/methods-models.csv", strip.white = TRUE,
  stringsAsFactors = FALSE) %>% dplyr::filter(show == "yes")
terms <- unique(d$gram)
d$gram <- tolower(d$gram)

N <- unlist(lapply(strsplit(terms, " "), length))
terms3 <- terms[N==3]
out1 <- gram_db1 %>% filter(gram %in% terms) %>% collect(n = Inf) %>% inner_join(total1)
out2 <- gram_db2 %>% filter(gram %in% terms) %>% collect(n = Inf) %>% inner_join(total1)
out3 <- get_ngram_dat(terms3)

saveRDS(out, file = "data/generated/method-grams.rds")
out <- readRDS("data/generated/method-grams.rds")
d <- full_join(d, out, by = "gram") %>%
  filter(!is.na(total)) %>%
  filter(!is.na(show))
pdf("figs/stats.pdf", width = 6, height = 5)
ecogram_panels(d, right_gap = 50, ncols = 2)
dev.off()

pdf("figs/stats1.pdf", width = 6, height = 5)
ecogram_panels(d, right_gap = 50, pal = pal_func)
dev.off()

# --------

# Brent:
d <- read.csv("data/conservation-terms.csv", strip.white = TRUE, stringsAsFactors = FALSE)
terms <- unique(d$gram)
d$gram <- tolower(d$gram)
# out <- get_ngram_dat(terms)
unlist(lapply(strsplit(terms, " "), length))
# out <- readRDS("data/generated/conservation-grams.rds")
# saveRDS(out, file = "data/generated/conservation-grams.rds")
# d$gram[!d$gram %in% out$gram]
out1 <- gram_db1 %>% filter(gram %in% terms) %>% collect(n = Inf) %>% inner_join(total1)
out2 <- gram_db2 %>% filter(gram %in% terms) %>% collect(n = Inf) %>% inner_join(total1)
out <- bind_rows(out1, out2) %>% unique()
saveRDS(out, file = "data/generated/conservation-grams.rds")
out <- readRDS("data/generated/conservation-grams.rds")

d <- full_join(d, out, by = "gram") %>%
  filter(!is.na(total)) %>%
  dplyr::filter(show == "yes")
pdf("figs/conservation-panels-3.pdf", width = 6, height = 3.9)
ecogram_panels(d, right_gap = 38, ncols = 2, pal = pal_func)
dev.off()


# --------
# Paul:
d <- read.csv("data/paul_human_impacts3.csv", strip.white = TRUE, stringsAsFactors = FALSE)
terms <- unique(d$gram)
d$gram <- tolower(d$gram)
d$show <- "yes"
# out <- get_ngram_dat(terms)
# saveRDS(out, file = "data/generated/human-impacts-grams.rds")
# out <- readRDS("data/generated/human-impacts-grams.rds")
d$gram[!d$gram %in% out$gram]
N <- unlist(lapply(strsplit(terms, " "), length))
terms3 <- terms[N==3]

out1 <- gram_db1 %>% filter(gram %in% terms) %>% collect(n = Inf) %>% inner_join(total1)
out2 <- gram_db2 %>% filter(gram %in% terms) %>% collect(n = Inf) %>% inner_join(total1)
out3 <- get_ngram_dat(terms3)
out <- bind_rows(out1, out2) %>% bind_rows(out3) %>% unique()
saveRDS(out, file = "data/generated/conservation-grams.rds")
out <- readRDS("data/generated/conservation-grams.rds")
out <- filter(out, gram %in% d$gram)
d <- full_join(d, out, by = "gram") %>%
  filter(!is.na(total))

pdf("figs/human-impacts-panels-4.pdf", width = 6, height = 3.9)
ecogram_panels(d, right_gap = 38, ncols = 2, pal = pal_func)
dev.off()

pdf("figs/human-impacts-panels-5.pdf", width = 6, height = 3.9)
ecogram_panels(d, right_gap = 38, ncols = 2)
dev.off()

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
  filter(!is.na(total)) %>%
  dplyr::filter(show == "yes")
pdf("figs/scale-panels-2.pdf", width = 6, height = 5)
ecogram_panels(d, right_gap = 50, ncols = 2)
dev.off()
