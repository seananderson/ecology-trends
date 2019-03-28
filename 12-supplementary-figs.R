library(tidyverse)
library(ggsidekick)
library(forcats)

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
  filter(year >= 1930, year <= 2010) %>%
  filter(!(gram %in% bad_amnat & journal == "amernatu")) %>%
  filter(!pub_id %in% pnas_exluded_pub_ids,
    journal %in% filtered_journals$journal)

if (!file.exists("data/generated/journal_totals_all.rds")) {
  journal_totals <- ngrams1 %>%
    group_by(journal, year) %>%
    summarize(total_words = sum(count)) %>%
    collect(n = Inf)
  saveRDS(journal_totals, file = "data/generated/journal_totals_all.rds")
} else {
  journal_totals <- readRDS("data/generated/journal_totals_all.rds")
}


g <- journal_totals %>%
  group_by(journal) %>%
  mutate(total_journal_words = sum(total_words)) %>%
  ggplot(aes(year, total_words/1e6)) +
  geom_line() +
  facet_wrap(~forcats::fct_reorder(journal, -total_journal_words),
    scales = "fixed") + ggsidekick::theme_sleek() +
  ylab("Total words (millions)") + xlab("Year")

ggsave("figs/journal-totals-by-year-all.pdf", width = 10, height = 9)
