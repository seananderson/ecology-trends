library(tidyverse)
d <- readr::read_csv("data/paul-2017-12-13.csv", trim_ws = TRUE)
d <- mutate(d, term = gsub("\\*", "", term))
d <- filter(d, !is.na(term))

terms <- d$term
terms <- union(terms, gsub("-", " ", terms[grepl("-", terms)]))
terms <- union(terms, gsub("-", "", terms[grepl("-", terms)]))
terms <- terms[!terms %in% c("landuse change")]

library("koRpus")
set.kRp.env(TT.cmd =
    "~/Dropbox/bin/treetagger/cmd/tree-tagger-english", lang = "en")
tt <- treetag(terms, format = "obj")
tt <- tt@TT.res
plurals <- unique(filter(tt, tag == "NNS")$token)

x <- unlist(sapply(plurals, function(x) d$term[grepl(x, d$term)]))
names(x) <- NULL
x <- gsub("s$", "", x)
terms <- union(terms, x)
terms <- sort(terms)

source("analysis/extract-functions.R")

N <- unlist(lapply(strsplit(terms, " "), length))
out1 <- get_ngram_dat(terms[N == 1])
out2 <- get_ngram_dat(terms[N == 2])
out3 <- get_ngram_dat(terms[N == 3])

dat <- bind_rows(list(out1, out2, out3)) %>%
  ungroup()
dat <- left_join(dat, rename(d, gram = term, grouped_terms = `grouped terms`)) %>%
  arrange(gram, year)
saveRDS(dat, file = "data/generated/paul-2017-12-13.rds")
