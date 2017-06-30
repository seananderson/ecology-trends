library(tidyverse)
library(ggsidekick)
library(forcats)

dd <- dplyr::src_sqlite("data/jstor1.sqlite3")
temp <- dplyr::tbl(dd, "ngrams") %>% filter(year != 999)

total1 <- temp %>% group_by(year) %>%
  summarise(total_words = sum(count)) %>%
  collect()
# ggplot(total1, aes(year, total_words)) + geom_point()

ecology <- temp %>% filter(gram == "ecology") %>%
  group_by(year) %>%
  summarise(ecology = sum(count)) %>%
  collect()
ecology <- inner_join(total1, ecology)

ggplot(ecology, aes(year, ecology/total_words)) +
  geom_point() +
  geom_smooth(colour = "red", method = "gam",
    method.args = list(family = Gamma(link = "log")),
    formula = y ~ s(x)) +
  theme_light()

ggsave("figs/ecology-example.pdf", width = 6, height = 5)

terms <- read_csv("data/Grouped-terms-Sheet1.csv")
terms <- terms[unlist(lapply(strsplit(terms$Term, " "), length)) == 1, ]
terms <- terms$Term

ecology <- temp %>% filter(gram %in% terms) %>%
  group_by(year, gram) %>%
  summarise(total = sum(count)) %>%
  collect()
ecology <- inner_join(total1, ecology)

g <- filter(ecology, year <= 2014) %>%
  group_by(gram) %>%
  mutate(total_count = sum(total)) %>%
  ggplot(aes(year, ((total+0.5)/total_words))) +
  geom_point(alpha = 0.7, colour = "grey40") +
  geom_smooth(colour = "red", method = "gam",
    method.args = list(family = Gamma(link = "log")),
    formula = y ~ s(x), se = FALSE) +
  theme_sleek() +
  facet_wrap(~fct_reorder(gram, -total_count), scales = "free_y")

ggsave("figs/trends-example.pdf", width = 22, height = 17)
