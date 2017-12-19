source("analysis/extract-functions.R")

blank_grams <- ngrams2 %>%
  # filter(year %in% c(1930:2014)) %>%
  filter(
    gram %like% "% ecology" |
      gram %like% "% experiment" |
      gram %like% "% experiments" |
      gram %like% "% data" |
      gram %like% "% model" |
      gram %like% "% models" |
      gram %like% "% analysis" |
      gram %like% "% variation" |
      gram %like% "% variability" |
      gram %like% "% rate" |
      gram %like% "% rates" |
      gram %like% "% scale" |
      gram %like% "% autocorrelation" |
      gram %like% "% pattern" |
      gram %like% "% patterns" |
      gram %like% "% ecosystem" |
      gram %like% "% ecosystems" |
      gram %like% "% diversity" |
      gram %like% "% population" |
      gram %like% "% populations" |
      gram %like% "% distribution" |
      gram %like% "% management" |
      gram %like% "% conservation" |
      gram %like% "ecological %" |
      gram %like% "%ical"
  ) %>%
  group_by(year, gram) %>%
  summarise(total = sum(count)) %>%
  collect(n = Inf) %>%
  ungroup()

save(blank_grams, file = "data/generated/blank_grams.rda")
load("data/generated/blank_grams.rda")
blank_grams <- left_join(blank_grams, total1, by = "year")

## fix below 2000 - no 2000 + 1940:

library("koRpus")
set.kRp.env(TT.cmd =
    "~/Dropbox/bin/treetagger/cmd/tree-tagger-english", lang = "en")
# tt <- treetag(terms, format = "obj")

ical <- filter(blank_grams, grepl("ical$", gram))
blank_grams <- filter(blank_grams, !gram %in% ical$gram)

x <- blank_grams %>%
  mutate(second_word = stringr::str_split(gram, " ", simplify = TRUE)[,2]) %>%
  mutate(first_word = stringr::str_split(gram, " ", simplify = TRUE)[,1])

eco <- filter(x, first_word == "ecological")
eco <- rename(eco, x_word = second_word) %>%
  rename(second_word = first_word) %>%
  rename(first_word = x_word)

x <- x %>% filter(second_word %in%
    c("ecology",
      "experiment",
      "experiments",
      "data",
      "model",
      "models",
      "analysis",
      "variation",
      "variability",
      "rate",
      "rates",
      "scale",
      "autocorrelation",
      "pattern",
      "patterns",
      "ecosystem",
      "ecosystems",
      "diversity",
      "population",
      "populations",
      "distribution",
      "management",
      "conservation"))

x <- bind_rows(x, eco)
x <- rename(x, panel = second_word)
nrow(x)
x <- filter(x, nchar(first_word) >= 3, !grepl("[0-9]+", first_word))
nrow(x)

x$first_word <- gsub("'", "", x$first_word)
x$gram <- gsub("'", "", x$gram)

yrs <- diff(range(x$year))
x_top <- x %>%
  filter(year >= 1935, year <= 2011) %>%
  # filter(year >= 1940, year <= 1949) %>%
  group_by(panel, gram, first_word) %>%
  summarise(total = sum(total)) %>%
  # summarise(total = sum(total/total_words)/yrs) %>%
  # summarise(total = mean(total/total_words), nyears = length(unique(year))) %>%
  arrange(panel, -total) %>%
  ungroup() %>%
  # filter(nyears >= 5) %>% # Check
  group_by(panel) %>%
  top_n(n = 150, wt = total)

x_top2 <- x_top %>%
  filter(nchar(first_word) >= 4) %>%
  mutate(first_word_type = treetag(first_word, format = "obj")@TT.res$wclass) %>%
  filter(first_word_type %in% c("adjective", "noun")) %>%
  mutate(lemma = treetag(first_word, format = "obj")@TT.res$lemma) %>%
  group_by(panel) %>%
  top_n(n = 40, wt = total)

filter(x_top2, lemma == "<unknown>") %>% as.data.frame()
x_top2 <- filter(x_top2, !first_word %in% c("ecol",
  "tial", "i.e", "ltd", "poral", "tial", "authors", "way",
  "these", "great", "other", "term", "cine", "tbe",
  "affect", "editors", "same", "such", "cer", "esw", "l.f", "r.f"))
x_top2$lemma[x_top2$first_word == "unpubl"] <- "unpublished"
x_top2$lemma[x_top2$lemma == "<unknown>"] <- x_top2$first_word[x_top2$lemma == "<unknown>"]

x_top2 <- mutate(x_top2,
  panel_lemma = treetag(panel, format = "obj")@TT.res$lemma)

top_lemmas <- group_by(x_top2, panel_lemma, lemma) %>%
  summarise(total = sum(total)) %>%
  group_by(panel_lemma) %>%
  top_n(n = 20, wt = total)

terms <- filter(x_top2, lemma %in% top_lemmas$lemma)

gram_dat <- get_ngram_dat(terms$gram)
save(gram_dat, file = "data/generated/top-blank.rda")

# gram_dat_old <- get_ngram_dat(terms$gram)
# save(gram_dat_old, file = "data/generated/top-blank-old.rda")

# gd <- inner_join(gram_dat, rename(terms, total_2000s = total),
gd <- inner_join(gram_dat_old, rename(terms, total_2000s = total),
  by = "gram")
gd <- group_by(gd, year, panel_lemma, lemma, total_words) %>%
  summarise(total = sum(total), total_2000s = sum(total_2000s)) %>%
  ungroup()

top_lemmas_second_cut <- group_by(top_lemmas, panel_lemma, lemma) %>%
  summarise(total = sum(total)) %>%
  filter(!lemma %in% c("first", "second")) %>%
  ungroup() %>%
  group_by(panel_lemma) %>%
  top_n(n = 8, wt = total)

gd <- inner_join(gd, select(top_lemmas_second_cut, -total),
  by = c("panel_lemma", "lemma"))

make_panel <- function(dat, lab_dat, title) {
  g <- dat %>%
    filter(year <= 2011, year > 1930) %>%
    ggplot(aes(year, total/total_words*10000, group = lemma)) +
    geom_line(colour = "grey30", alpha = 0.2) +
    geom_col(colour = NA, fill = NA, position = position_dodge()) +
    # facet_wrap(~panel, scales = "free_y") +
    geom_smooth(method = "gam",
      method.args = list(family = gaussian(link = "identity")),
      formula = y ~ s(x), se = FALSE,
      aes(colour = lemma), lwd = 1.25) +
    ggsidekick::theme_sleek() +
    scale_color_brewer(palette = "Set2") +
    ggrepel::geom_text_repel(data = lab_dat,
      aes_string(y = "y", label = "lemma", colour = "as.factor(lemma)"),
      size = 4,
      nudge_x = 12,
      segment.size = 0.2,
      segment.color = "#00000030"
    ) +
    scale_x_continuous(breaks = seq(1920, 2012, 20), limits = c(1935, 2038)) +
    guides(colour = FALSE, lty = FALSE) +
    ylab("Instances per 10,000 words") + xlab("") +
    ggtitle(title)
}

out <- plyr::dlply(gd, "panel_lemma", function(xx) {
  lab <- plyr::ddply(xx, c("panel_lemma", "lemma"), function(x) {
    xx <- filter(x, year <= 2011, year > 1930)
    m <- tryCatch({mgcv::gam(total/total_words*10000 ~ s(year), data = xx)},
      error = function(e) NA)
    y <- ifelse(!is.na(m)[[1]], predict(m, newdata = data.frame(year = 2011))[[1]], NA)
    data.frame(year = 2011, y = y)
  })

  make_panel(xx, lab, unique(xx$panel))
})

# pdf("figs/blank-panels2.pdf", width = 6, height = 9)
# for (i in seq_along(out)) {
#   print(out[[i]])
# }
# dev.off()

library(gridExtra)
pdf("figs/blank-panels-mean.pdf", width = 20, height = 23)
n <- length(out)
nCol <- floor(sqrt(n))
do.call("grid.arrange", c(out, ncol=nCol))
dev.off()

# pdf("figs/blank-panels1.pdf", width = 12, height = 9)
# gridExtra::grid.arrange(out[[1]], out[[2]], out[[3]], out[[4]])
# dev.off()

# This is the top 8 *blank* data/ecology/experiment/model lemmas/standardized
# ngrams from
# 2000-2010 potted over time where *blank* is a noun or adjective.
#
# Look at the peak and then sharp decline of unpublished data, it may be
# coinciding with the Internet and digital journal processes?
#
# Field experiments peaked around 2000.
#
# Null models are the most common type of 'model' after around 2000 but
# are clearly on the decline. I think they've experienced a fair bit of
# backlash.
#
# Simple models used to be king but are dwarfed by other types now and
# are on the decline.
#
# Global models weren't really a thing until the 1990s but are now in the top 8.
#
# Landscape ecology was the second last giant of the modern ecologies to
# develop. Global ecology is the most recent. Functional ecology dwarfs
# everything... I wonder how much journal names influence this panel.
#
# Empirical data caught up to field data in 2011.
#
# Will be neat to look at the same plots with the most popular stuff
# from the 1940s.
#
# 'garden' experiments are on the rise... I wonder how much of that is
# 'common garden'?
#
# You can also see the increasing popularity of comparing multiple
# models with the growth of "full model" and "final model" from 1990ish
# on.
