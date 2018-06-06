source("analysis/extract-functions.R")
source("analysis/pretty-panels.R")

gram_db2 <- dplyr::src_sqlite("data/generated/jstor2-condensed.sqlite3") %>%
  dplyr::tbl("ngrams")

blank_grams <- gram_db2 %>%
  filter(year %in% c(1930:2010)) %>%
  filter(
      gram %like% "% experiment" |
      gram %like% "% experiments" |
      # gram %like% "% data" |
      # gram %like% "% model" |
      # gram %like% "% models" |
      gram %like% "% analysis" |
      # gram %like% "% variation" |
      gram %like% "% variability" |
      # gram %like% "% scale" |
      gram %like% "% ecosystem" |
      gram %like% "% ecosystems" |
      gram %like% "% diversity" |
      gram %like% "% population" |
      gram %like% "% populations" |
      gram %like% "% distribution" |
      gram %like% "% conservation"
  ) %>%
  # group_by(year, gram) %>%
  # summarise(total = sum(count)) %>%
  collect(n = Inf)

second_words <- c("experiment", "experiments", "analysis", "variability",
  "ecosystem", "ecosystems", "diversity", "population", "populations",
  "distribution", "conservation")

save(blank_grams, file = "data/generated/blank_grams.rda")
load("data/generated/blank_grams.rda")

blank_grams <- left_join(blank_grams, total1, by = "year")

x <- blank_grams %>%
  mutate(second_word = stringr::str_split(gram, " ", simplify = TRUE)[,2]) %>%
  mutate(first_word = stringr::str_split(gram, " ", simplify = TRUE)[,1]) %>%
  mutate(panel = second_word)

# in case the cached data includes words that are no longer used:
x <- filter(x, second_word %in% second_words)

x <- filter(x, nchar(first_word) >= 4, !grepl("[0-9]+", first_word))

x_top <- x %>%
  filter(year >= 1930, year <= 2010) %>%
  filter(!grepl("\\.", first_word)) %>%
  filter(!grepl("\\:", first_word)) %>%
  filter(!grepl("\\_", first_word)) %>%
  filter(!grepl("\\'", first_word)) %>%
  group_by(panel, gram, first_word) %>%
  summarise(total = sum(total)) %>%
  arrange(panel, -total) %>%
  group_by(panel) %>%
  top_n(n = 60, wt = total)

x_top2 <- mutate(x_top,
  first_word_type = treetag(first_word, format = "obj")@TT.res$wclass) %>%
  filter(first_word_type %in% c("adjective", "noun")) %>%
  mutate(lemma = treetag(first_word, format = "obj")@TT.res$lemma)

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
  filter(!(panel_lemma == "conservation" & lemma == "biological")) %>%
  filter(!(panel_lemma == "experiment" & lemma == "present")) %>%
  filter(!(panel_lemma == "experiment" & lemma == "previous")) %>%
  summarise(total = sum(total)) %>%
  group_by(panel_lemma) %>%
  top_n(n = 30, wt = total)

terms <- filter(x_top2, lemma %in% top_lemmas$lemma)
grams <- unique(terms$gram)

gram_dat <- gram_db2 %>%
  filter(year %in% c(1930:2010)) %>%
  filter(gram %in% grams) %>%
  collect(n = Inf) %>%
  left_join(total1, by = "year")

save(gram_dat, file = "data/generated/top-blank.rda")
load("data/generated/top-blank.rda")

gd <- inner_join(gram_dat, rename(terms, total_over_time = total), by = "gram")
gd <- group_by(gd, year, panel_lemma, lemma, total_words) %>%
  summarise(total = sum(total), total_over_time = sum(total_over_time)) %>%
  ungroup()

top_lemmas_second_cut <- group_by(top_lemmas, panel_lemma, lemma) %>%
  summarise(total = sum(total)) %>%
  filter(!lemma %in% c("first", "second")) %>%
  ungroup() %>%
  group_by(panel_lemma) %>%
  top_n(n = 8, wt = total)

gd <- inner_join(gd, select(top_lemmas_second_cut, -total),
  by = c("panel_lemma", "lemma"))

gd <- gd %>% mutate(lemma = gsub("specie", "species", lemma))
gd <- gd %>% mutate(lemma = gsub("datum", "data", lemma))
gd <- gd %>% mutate(panel_lemma = gsub("datum", "data", panel_lemma))
gd <- gd %>% filter(panel_lemma != "scale")

plot_blanks <- function(dat, right_gap = 40,
  label_cex = 0.85, ...) {
  dat <- dat %>%
    mutate(gram_canonical = lemma, panel = panel_lemma) %>%
    arrange(panel, gram_canonical, year)
  n <- length(unique(dat$panel))
  ncols <- floor(sqrt(n))
  nrows <- ceiling(n/ncols)
  par(mfrow = c(nrows, ncols))
  par(mgp = c(2, 0.3, 0), tcl = -0.15, las = 1, cex = 0.7,
    col.axis = "grey55", mar = c(0.025, 2.1, 0, 0), oma = c(1.7, 1.1, .5, .5))
  ii <<- 1
  xaxes <- seq(n - (ncols - 1), n)
  mutate(dat, total_words = total_words/1e5, total = total) %>%
    plyr::d_ply("panel", function(x) {
      ecogram_panel(x, xaxes = xaxes,
        right_gap = right_gap, label_cex = label_cex, yfrac_let = 0.06,
        lab_text = paste(simple_cap(as.character(unique(x$panel))),
          collapse = ""), ...)})
  mtext("Frequency per 100,000 words", side = 2, outer = TRUE, line = -.4,
    col = "grey45", cex = 0.85, las = 0)
}

gd$panel_lemma <- factor(gd$panel_lemma, levels = c(
  "conservation",
  "ecosystem",
  "population",
  "diversity",
  "variability",
  "distribution",
  "experiment",
  "analysis"))

gold <- 0.618
pdf("figs/blanks-viridis2.pdf", width = 6.5, height = 6.5 * 2 * gold * 1.03)
gd %>%
  plot_blanks(right_gap = 34, log_y = FALSE,
    bottom_frac_up = 0.02, label_gap = -1.0,
    show_seg = TRUE, pal = pal_func)
dev.off()

pdf("figs/blanks2.pdf", width = 6.5, height = 6.5 * 2 * gold * 1.03)
gd %>%
  plot_blanks(right_gap = 34, log_y = FALSE,
    bottom_frac_up = 0.02, label_gap = -1.0,
    show_seg = TRUE)
dev.off()

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
