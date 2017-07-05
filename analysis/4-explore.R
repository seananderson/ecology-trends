library(tidyverse)
library(ggsidekick)
library(forcats)
library(directlabels)

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

# ----------------

make_fig <- function(group) {
  filter(dat, Group %in% group, year <= 2011, year >= 1935) %>%
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
    ylab("Instances per 'ecology'") +
    ggtitle(group)
}

# ----------

terms <- read_csv("data/Grouped-terms-Sheet1.csv")
terms <- rename(terms, gram = Term)
dat <- left_join(bind_rows(ecology, ecology2), terms)
g <- make_fig("ecology")
ggsave("figs/ecology-group.pdf", width = 10, height = 8)

span <- 0.75

dd <- filter(dat, Group %in% "ecology", year <= 2011, year >= 1935) %>%
  ungroup() %>%
  group_by(gram) %>%
  mutate(max_count = max(total/total_words))

library(mgcv)
dd2 <- plyr::ddply(dd, "gram", function(x) {
  if (nrow(x) > 10) {
    m <- gam(I(log10(total/total_words*1000)) ~ s(year), data = x)
  } else {
    m <- gam(I(log10(total/total_words*1000)) ~ s(year, k = 3), data = x)
  }
  data.frame(year = x$year, pred = predict(m))
})


ggplot(dd2, aes(year, pred, group = gram, colour = gram)) +
  # geom_point(alpha = 0.1, colour = "grey40", cex = 0.5) +
  # geom_smooth(se = FALSE) +
  # geom_smooth(colour = "red", method = "gam",
  #   method.args = list(family = Gamma(link = "log")),
  #   formula = y ~ s(x), se = FALSE) +
  theme_sleek() +
  geom_line() +
  # facet_wrap(~fct_reorder(gram, -max_count), scales = "free_y") +
  ylab("log10(Instances per 1000 'ecology')") +
  ggtitle("ecology") +
  xlim(1925, 2025) +
  # stat_smooth(span=span, se=F) +
  scale_colour_discrete(guide = 'none') +
  # viridis::scale_colour_viridis(discrete = TRUE) +
  geom_dl(aes(label = gram), method = list(dl.combine("first.points", "last.points"), cex = 0.7))
ggsave("figs/ecology-group.pdf", width = 9, height = 8)
