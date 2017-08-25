library(tidyverse)

d <- dplyr::src_sqlite("data/jstor1.sqlite3")
d <- dplyr::tbl(d, "ngrams") %>% filter(year != 999)

pnas_ecology <- d %>% filter(journal %in% "procnatiacadscie") %>%
  filter(gram %in% c("ecology", "ecological")) %>% collect()
saveRDS(pnas_ecology, "data/generated/pnas_ecology.rds")

pnas_exludes <- d %>% filter(journal %in% "procnatiacadscie") %>%
  filter(!pub_id %in% pnas_ecology$pub_id) %>%
  select(pub_id) %>%
  collect()
pnas_exluded_pub_ids <- unique(pnas_exludes$pub_id)
saveRDS(pnas_exluded_pub_ids, "data/generated/pnas_exluded_pub_ids.rds")

annual_journal_counts <- d %>%
  filter(!pub_id %in% pnas_exluded_pub_ids) %>%
  group_by(journal, year) %>%
  summarise(total_words = sum(count)) %>%
  collect()
saveRDS(annual_journal_counts, "data/generated/annual_journal_counts.rds")

g <- annual_journal_counts %>%
  mutate(total_journal_words = sum(total_words)) %>%
  ggplot(aes(year, total_words/1e6)) +
  geom_line() +
  facet_wrap(~forcats::fct_reorder(journal, -total_journal_words),
    scales = "fixed")
ggsave("figs/journals.pdf", width = 15, height = 12)

g <- annual_journal_counts %>%
  mutate(total_journal_words = sum(total_words)) %>%
  ggplot(aes(year, total_words/1e6, group = journal)) +
  geom_line() +
  facet_wrap(~forcats::fct_reorder(journal, -total_journal_words),
    scales = "free_y")
ggsave("figs/journals-free.pdf", width = 18, height = 12)
