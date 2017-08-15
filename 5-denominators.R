library(tidyverse)

d <- dplyr::src_sqlite("data/jstor1.sqlite3")
d <- dplyr::tbl(d, "ngrams") %>% filter(year != 999)

x <- d %>%
  group_by(journal, year) %>%
  summarise(total_words = sum(count)) %>%
  collect()

g <- x %>%
  mutate(total_journal_words = sum(total_words)) %>%
  ggplot(aes(year, total_words/1e6)) +
  geom_line() +
  facet_wrap(~forcats::fct_reorder(journal, -total_journal_words),
    scales = "fixed")
ggsave("figs/journals.pdf", width = 15, height = 12)

g <- x %>%
  mutate(total_journal_words = sum(total_words)) %>%
  ggplot(aes(year, total_words/1e6, group = journal)) +
  geom_line() +
  facet_wrap(~forcats::fct_reorder(journal, -total_journal_words),
    scales = "free_y")
ggsave("figs/journals-free.pdf", width = 18, height = 12)
