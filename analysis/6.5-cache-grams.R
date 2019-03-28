library(tidyverse)
# source("analysis/extract-functions.R")

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
  # filter(TaxonSpecific == "N") %>%
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

ng1 <- ngrams1 %>%
  filter(year >= 1920, year <= 2011) %>%
  group_by(gram, year) %>%
  summarise(total = sum(count)) %>%
  collect(n = Inf)
saveRDS(ng1, file = "data/generated/condensed-ngrams1.rds")
rm(ng1)

ng2 <- ngrams2 %>%
  filter(year >= 1920, year <= 2011) %>%
  group_by(gram, year) %>%
  summarise(total = sum(count)) %>%
  collect(n = Inf) %>%
  filter(!grepl("[0-9]+", gram)) # no numbers
saveRDS(ng2, file = "data/generated/condensed-ngrams2.rds")
rm(ng2)

d2 <- readRDS("data/generated/condensed-ngrams2.rds")
db2 <- src_sqlite("data/generated/jstor2-condensed.sqlite3", create = TRUE)
copy_to(db2, d2, "ngrams", temporary = FALSE)
rm(d2)

d1 <- readRDS("data/generated/condensed-ngrams1.rds")
db1 <- src_sqlite("data/generated/jstor1-condensed.sqlite3", create = TRUE)
copy_to(db1, d1, "ngrams", temporary = FALSE)
rm(d1)
