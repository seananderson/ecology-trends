library(tidyverse)
library(ggsidekick)
library(forcats)
library(directlabels)

pnas_exluded_pub_ids <- readRDS("data/generated/pnas_exluded_pub_ids.rds")

filtered_journals <-
  readr::read_csv(
    "data/taxa-specific-journal-classifications.csv",
    col_types = cols(
      Journal = col_character(),
      Slug = col_character(),
      TaxonSpecific = col_character(),
      Taxon = col_character(),
      Notes = col_character()
    )
  ) %>%
  filter(TaxonSpecific == "N") %>%
  select(Slug) %>% rename(journal = Slug)

bad_amnat <- readr::read_lines("data/bad-amnat.txt") # latex code

d1 <- dplyr::src_sqlite("data/jstor1.sqlite3")
ngrams1 <- dplyr::tbl(d1, "ngrams") %>% filter(year != 999) %>%
  filter(year >= 1920, year <= 2014) %>%
  filter(!(gram %in% bad_amnat & journal == "amernatu")) %>%
  filter(!pub_id %in% pnas_exluded_pub_ids,
    journal %in% filtered_journals$journal)

d2 <- dplyr::src_sqlite("data/jstor2.sqlite3")
ngrams2 <- dplyr::tbl(d2, "ngrams") %>% filter(year != 999) %>%
  filter(year >= 1920, year <= 2014) %>%
  filter(!pub_id %in% pnas_exluded_pub_ids,
    journal %in% filtered_journals$journal)

d3 <- dplyr::src_sqlite("data/jstor3.sqlite3")
ngrams3 <- dplyr::tbl(d3, "ngrams") %>% filter(year != 999) %>%
  filter(year >= 1920, year <= 2014) %>%
  filter(!pub_id %in% pnas_exluded_pub_ids,
    journal %in% filtered_journals$journal)

if (!file.exists("data/generated/total1.rds")) {
  total1 <- ngrams1 %>%
    group_by(year) %>%
    summarise(total_words = sum(count)) %>%
    collect(n = Inf)
  saveRDS(total1, file = "data/generated/total1.rds")
} else {
  total1 <- readRDS("data/generated/total1.rds")
}

get_ngram_dat <- function(terms) {

  terms <- data.frame(terms = as.character(terms), stringsAsFactors = FALSE)
  terms$n <- unlist(lapply(strsplit(terms$terms, " "), length))
  terms$terms <- tolower(terms$terms)
  terms <- mutate(terms,
    terms = sub("^[ ]+", "", terms),
    terms = sub("[ ]+$", "", terms))
  terms <- terms[!duplicated(terms), ]

  if (nrow(filter(terms, n > 3)) > 1) warning("Some > 3 grams")

  ecology1 <- ecology2 <- ecology3 <- data.frame(year = NA, gram = NA,
    total = NA, total_words = NA)

  dd <- filter(terms, n == 1)
  if (nrow(dd) > 0) {
    message("Extracting 1 grams")
    ecology1 <- ngrams1 %>% filter(gram %in% dd$terms) %>%
      group_by(year, gram) %>%
      summarise(total = sum(count)) %>%
      collect(n = Inf) %>%
      left_join(total1, by = "year")
  }

  dd <- filter(terms, n == 2)
  if (nrow(dd) > 0) {
    message("Extracting 2 grams")
    ecology2 <- ngrams2 %>% filter(gram %in% dd$terms) %>%
      group_by(year, gram) %>%
      summarise(total = sum(count)) %>%
      collect(n = Inf) %>%
      left_join(total1, by = "year")
  }

  dd <- filter(terms, n == 3)
  if (nrow(dd) > 0) {
    message("Extracting 3 grams")
    ecology3 <- ngrams3 %>% filter(gram %in% dd$terms) %>%
      group_by(year, gram) %>%
      summarise(total = sum(count)) %>%
      collect(n = Inf) %>%
      left_join(total1, by = "year")
  }
  dat <- bind_rows(list(ecology1, ecology2, ecology3)) %>%
    ungroup()
  filter(dat, !is.na(gram))
}

simple_cap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1, 1)), substring(s, 2),
    sep = "", collapse = " ")
}

library("koRpus")
library("koRpus.lang.en")
set.kRp.env(TT.cmd =
    "~/Dropbox/bin/treetagger/cmd/tree-tagger-english", lang = "en")

gram_db1 <- dplyr::src_sqlite("data/generated/jstor1-condensed.sqlite3") %>%
  dplyr::tbl("ngrams")
gram_db2 <- dplyr::src_sqlite("data/generated/jstor2-condensed.sqlite3") %>%
  dplyr::tbl("ngrams")

get_speech_part <- function(.x) {
  # as.character(vapply(.x,
    # function(xx)
      # as.character(koRpus::treetag(xx, format = "obj")@TT.res$wclass), character(1)))

  koRpus::treetag(paste0(.x, "."), format = "obj")@TT.res %>% filter(lemma != ".") %>%
    pull(wclass)
}

get_lemma <- function(.x) {
  # as.character(vapply(.x,
  #   function(xx)
  #     as.character(koRpus::treetag(xx, format = "obj")@TT.res$lemma), character(1)))

  koRpus::treetag(paste0(.x, "."), format = "obj")@TT.res %>% filter(lemma != ".") %>%
    pull(lemma)
}
