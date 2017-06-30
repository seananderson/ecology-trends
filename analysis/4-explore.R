library(tidyverse)
library(ggsidekick)
library(forcats)

dd <- dplyr::src_sqlite("data/jstor1.sqlite3")
temp <- dplyr::tbl(dd, "ngrams") %>% filter(year != 999)

d2 <- dplyr::src_sqlite("data/jstor2.sqlite3")
temp2 <- dplyr::tbl(d2, "ngrams") %>% filter(year != 999)

total1 <- filter(temp, gram == "ecology") %>%
  group_by(year) %>%
  summarise(total_words = sum(count)) %>%
  collect()
ggplot(total1, aes(year, total_words)) + geom_point()

# ---------
terms <- read_csv("data/Grouped-terms-Sheet1.csv")
terms <- terms[unlist(lapply(strsplit(terms$Term, " "), length)) == 1, ]
terms <- terms$Term

ecology <- temp %>% filter(gram %in% terms) %>%
  group_by(year, gram) %>%
  summarise(total = sum(count)) %>%
  collect(n = Inf) %>%
  inner_join(total1)

# ---------
terms <- read_csv("data/Grouped-terms-Sheet1.csv")
terms <- terms[unlist(lapply(strsplit(terms$Term, " "), length)) == 2, ]
terms <- terms$Term

ecology2 <- temp2 %>% filter(gram %in% terms) %>%
  group_by(year, gram) %>%
  summarise(total = sum(count)) %>%
  collect(n = Inf) %>%
  inner_join(total1)

# ---------
g <- filter(ecology, year <= 2011, year >= 1935) %>%
  ungroup() %>%
  group_by(gram) %>%
  mutate(max_count = max(total/total_words)) %>%
  ggplot(aes(year, (total/total_words)*1)) +
  geom_point(alpha = 0.7, colour = "grey40") +
  geom_smooth(colour = "red", method = "gam",
    method.args = list(family = Gamma(link = "log")),
    formula = y ~ s(x), se = FALSE) +
  theme_sleek() +
  facet_wrap(~fct_reorder(gram, -max_count), scales = "free_y") +
  ylab("Instances per 'ecology'")

ggsave("figs/trends-example-ecology.pdf", width = 22, height = 17)

# ---------

g <- filter(ecology2, year <= 2011, year >= 1935) %>%
  ungroup() %>%
  group_by(gram) %>%
  mutate(max_count = max(total/total_words)) %>%
  ggplot(aes(year, (total/total_words)*1)) +
  geom_point(alpha = 0.7, colour = "grey40") +
  geom_smooth(colour = "red", method = "gam",
    method.args = list(family = Gamma(link = "log")),
    formula = y ~ s(x), se = FALSE) +
  theme_sleek() +
  facet_wrap(~fct_reorder(gram, -max_count), scales = "free_y") +
  ylab("Instances per 'ecology'")

ggsave("figs/trends2-example-ecology.pdf", width = 22, height = 17)
