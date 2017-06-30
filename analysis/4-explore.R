library(tidyverse)

dd <- dplyr::src_sqlite("data/jstor.sqlite3")
temp <- dplyr::tbl(dd, "ngrams") %>% filter(temp, year != 999)

total1 <- temp %>% group_by(year) %>%
  summarise(total_words = sum(count)) %>%
  collect()
ggplot(total1, aes(year, total_words)) + geom_point()

ecology <- temp %>% filter(gram == "ecology") %>%
  group_by(year) %>%
  summarise(ecology = sum(count)) %>%
  collect()
ecology <- inner_join(total1, ecology)

ggplot(ecology, aes(year, ecology/total_words)) +
  geom_point() +
  geom_smooth(colour = "red", method = "glm",
    method.args = list(family = Gamma(link = "log")))
