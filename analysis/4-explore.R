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

dd <- filter(dat, Group %in% "population_biology", year <= 2011, year >= 1935) %>%
  ungroup() %>%
  group_by(gram) %>%
  mutate(max_count = max(total/total_words))

dd <- group_by(dd, gram) %>%
  filter(total > 10) %>%
  mutate(n_year = n()) %>%
  ungroup() %>%
  filter(n_year >= 5)

library(mgcv)

dd2 <- plyr::ddply(dd, "gram", function(x) {
  m <- gam(I(log10(total/total_words*1000)) ~ s(year), data = x)
  mlm <- lm(I(log10(total/total_words*1000)) ~ year, data = x)
  y <- seq(min(x$year), max(x$year), 0.5)
  data.frame(year = y, pred = predict(m, newdata = data.frame(year = y)),
    slope = coef(mlm)[[2]])
})

# library(ggrepel)

labs_min <- group_by(dd2, gram) %>%
  summarise(pred = pred[year == min(year)],
    slope = slope[year == min(year)], year = min(year))
labs_max <- group_by(dd2, gram) %>%
  summarise(pred = pred[year == max(year)],
    slope = slope[year == max(year)], year = max(year))

ggplot(dd2, aes(year, pred, group = gram, colour = gram)) +
  # geom_point(alpha = 0.1, colour = "grey40", cex = 0.5) +
  # geom_smooth(se = FALSE) +
  # geom_smooth(colour = "red", method = "gam",
  #   method.args = list(family = Gamma(link = "log")),
  #   formula = y ~ s(x), se = FALSE) +
  theme_sleek() +
  geom_line(lwd = 0.8) +
  geom_text_repel(
    data = labs_min,
    aes(label = gram),
    size = 3,
    nudge_x = -5,
    segment.size = 0.2,
    segment.color = "#00000030"
  ) +
  geom_text_repel(
    data = labs_max,
    aes(label = gram),
    size = 3,
    nudge_x = 10,
    segment.size = 0.2,
    segment.color = "#00000030"
  ) +
  # facet_wrap(~fct_reorder(gram, -max_count), scales = "free_y") +
  ylab("log10(Instances per 1000 'ecology')") +
  ggtitle("population_biology") +
  xlim(1925, 2025) +
  # stat_smooth(span=span, se=F) +
  # scale_colour_discrete(guide = 'none') +
  # scale_color_gradient2(
    # low = scales::muted("blue"),
    # mid = "grey80",
    # high = scales::muted("red")) +
  scale_alpha_continuous(limits = c(-20, -2))
  # viridis::scale_colour_viridis(discrete = TRUE) +
  # geom_dl(aes(label = gram), method = list(dl.combine("first.points", "last.points"), cex = 0.8))
ggsave("figs/population_biology-group2.pdf", width = 15, height = 12)

# ------

d2 <- dplyr::src_sqlite("data/jstor2.sqlite3")
temp2 <- dplyr::tbl(d2, "ngrams") %>% filter(year != 999)

eco_grams <- temp2 %>% filter(year == 2008) %>%
  filter(gram %like% "% ecology") %>%
  collect(n = Inf)


eco_grams2 <- group_by(eco_grams, gram) %>%
  summarize(n = sum(count)) %>%
  filter(n > 25) %>%
  arrange(-n) %>%
  as.data.frame()

eco_grams2$first <- unlist(lapply(strsplit(eco_grams2$gram, " "), function(x) x[1]))

eco_grams2 <- filter(eco_grams2, !first %in% c("the", "a", "their", "and", "this", "al", "an", "of", "in", "for", "eds", "austral", "side", "one", "tional", "levels", "nities", "or", "usa", "alaska", "ed", "to", "on", "into", "america", "matters")) %>%
  filter(!grepl("[0-9]", first)) %>%
  filter(!grepl("^[a-zA-Z]$", first))

write_csv(eco_grams2, path = "ecologies.csv")

make_fig <- function(group) {
  filter(dat, gram %in% group, year <= 2011, year >= 1935) %>%
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

g <- make_fig(eco_grams$gram)
ggsave("figs/ecology-group.pdf", width = 10, height = 8)

