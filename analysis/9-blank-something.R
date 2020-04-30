# koRpus::install.koRpus.lang("en")
source("analysis/extract-functions.R")
source("analysis/pretty-panels.R")

gram_db2 <- dplyr::src_sqlite("data/generated/jstor2-condensed.sqlite3") %>%
  dplyr::tbl("ngrams")

blank_grams <- gram_db2 %>%
  filter(year %in% c(1930:2010)) %>%
  filter(
      gram %like% "% community" |
      gram %like% "% ecosystem" |
      gram %like% "% niche" |
      gram %like% "% species" |

      gram %like% "ecosystem %" |
      gram %like% "community %" |
      gram %like% "species %" |
      gram %like% "population %" |

      gram %like% "niche %" |
      gram %like% "% niche" |

      gram %like% "% theory" |

      gram %like% "% experiment" |
      gram %like% "% experiments" |
      gram %like% "% hypothesis" |
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

save(blank_grams, file = "data/generated/blank_grams.rda")
load("data/generated/blank_grams.rda")

blank_grams3 <- ngrams3 %>%
  filter(year %in% c(1930:2010)) %>%
  filter(
    gram %like% "theory of %"
  ) %>%
  group_by(year, gram) %>%
  summarise(total = sum(count)) %>%
  collect(n = Inf)

save(blank_grams3, file = "data/generated/blank_grams3.rda")
load("data/generated/blank_grams3.rda")

# in case old cached version with extra:
blank_grams3 <- blank_grams3 %>%
  filter(
    grepl("^theory of [0-9A-Za-z\\'\\-]+", gram)
  )

blank_grams3 <- blank_grams3 %>%
  mutate(gram = gsub("theory of", "theory-of", gram))
blank_grams <- bind_rows(blank_grams, blank_grams3)

blank_grams <- left_join(blank_grams, total1, by = "year")

x <- blank_grams %>%
  mutate(second_word = stringr::str_split(gram, " ", simplify = TRUE)[,2]) %>%
  mutate(first_word = stringr::str_split(gram, " ", simplify = TRUE)[,1]) %>%
  mutate(panel = second_word)

# swap some first and second words so second word is always panel:
orig_x <- x
x <- filter(x, !first_word %in%
    c("ecosystem", "community", "species", "population", "theory-of", "niche"))
swap <- filter(orig_x, first_word %in%
    c("ecosystem", "community", "species", "population", "theory-of", "niche"))
swap <- mutate(swap,
  temp_word = first_word,
  first_word = second_word,
  second_word = paste(temp_word, "as first"),
  panel = second_word
) %>% select(-temp_word)
x <- bind_rows(x, swap)
x <- unique(x)
rm(orig_x, swap)
assertthat::assert_that(length(unique(x$panel)) < 40)
assertthat::assert_that(length(unique(x$second_word)) < 40)

# in case the cached data includes words that are no longer used:
# x <- filter(x, second_word %in% second_words)

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
  first_word_type = get_speech_part(first_word)) %>%
  filter(first_word_type %in% c("adjective", "noun")) %>%
  mutate(lemma = get_lemma(first_word))

x_top2 <- mutate(x_top2, lemma = ifelse(lemma == "functioning", "function", lemma))

filter(x_top2, lemma == "<unknown>") %>% as.data.frame()
x_top2 <- filter(x_top2, !first_word %in% c("ecol",
  "tial", "i.e", "ltd", "poral", "tial", "authors", "way",
  "these", "great", "other", "term", "cine", "tbe",
  "affect", "editors", "same", "such", "cer", "esw", "l.f", "r.f"))
x_top2$lemma[x_top2$first_word == "unpubl"] <- "unpublished"
x_top2$lemma[x_top2$lemma == "<unknown>"] <- x_top2$first_word[x_top2$lemma == "<unknown>"]

x_top2 <- mutate(x_top2,
  panel_lemma =
    treetag(gsub(" as first", "", panel), format = "obj")@TT.res$lemma)
x_top2 <- x_top2 %>% mutate(panel_lemma =
    ifelse(grepl(" as first", panel), paste(panel_lemma, "as first"), panel_lemma))
x_top2 <- x_top2 %>% mutate(panel_lemma = gsub("^specie$", "species", panel_lemma))
x_top2 <- x_top2 %>% mutate(panel_lemma = gsub("^specie as first$",
  "species as first", panel_lemma))

top_lemmas <- group_by(x_top2, panel_lemma, lemma) %>%
  filter(!(panel_lemma == "conservation" & lemma == "biological")) %>%
  filter(!(panel_lemma == "species" & lemma == "many")) %>%
  filter(!(panel_lemma == "species" & lemma == "different")) %>%
  filter(!(panel_lemma == "experiment" & lemma == "present")) %>%
  filter(!(panel_lemma == "experiment" & lemma == "previous")) %>%
  summarise(total = sum(total)) %>%
  group_by(panel_lemma) %>%
  top_n(n = 30, wt = total)

terms <- filter(x_top2, lemma %in% top_lemmas$lemma)
save(terms, file = "data/generated/terms-top.rda")
save(top_lemmas, file = "data/generated/top-lemmas.rda")
grams <- unique(terms$gram)

grams2 <- grams[!grepl("theory-of", grams)]
grams3 <- gsub("-", " ", grams[grepl("theory-of", grams)])

gram_dat <- gram_db2 %>%
  filter(year %in% c(1930:2010)) %>%
  filter(gram %in% grams2) %>%
  collect(n = Inf) %>%
  left_join(total1, by = "year")

save(gram_dat, file = "data/generated/top-blank.rda")
load("data/generated/top-blank.rda")

gram_dat3 <- ngrams3 %>%
  filter(year %in% c(1930:2010)) %>%
  filter(gram %in% grams3) %>%
  group_by(year, gram) %>%
  summarise(total = sum(count)) %>%
  collect(n = Inf)

save(gram_dat3, file = "data/generated/top-blank3.rda")
load("data/generated/top-blank3.rda")

gram_dat3 <- left_join(gram_dat3, total1, by = "year")
gram_dat3 <- gram_dat3 %>% mutate(gram = gsub("theory of", "theory-of", gram))
gram_dat <- bind_rows(gram_dat, gram_dat3)

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

gd <- gd %>% mutate(lemma = gsub("specie$", "species", lemma))
gd <- gd %>% mutate(lemma = gsub("datum", "data", lemma))
gd <- gd %>% mutate(panel_lemma = gsub("datum", "data", panel_lemma))
gd <- gd %>% mutate(panel_lemma = gsub("<unknown> as first", "theory-of as first", panel_lemma))
gd <- gd %>% filter(panel_lemma != "scale")
gd <- gd %>% filter(panel_lemma != "hypothesis")

plot_blanks <- function(dat, right_gap = 40,
  label_cex = 0.85, yfrac_let = 0.08, one_pal = FALSE, ...) {
  dat <- dat %>%
    mutate(gram_canonical = lemma, panel = panel_lemma) %>%
    arrange(panel, gram_canonical, year)
  n <- length(unique(dat$panel))
  ncols <- floor(sqrt(n))
  nrows <- ceiling(n/ncols)
  par(mfrow = c(nrows, ncols))
  par(mgp = c(2, 0.3, 0), tcl = -0.15, las = 1, cex = 0.7,
    col.axis = "grey55", mar = c(0.025, 2.1, 0, 0), oma = c(1.7, 0.5, .5, .5))
  ii <<- 1
  xaxes <- seq(n - (ncols - 1), n)
  mutate(dat, total_words = total_words/1e5, total = total) %>%
    plyr::d_ply("panel", function(x) {
      if (!one_pal)
        pal <- if (ii %in% c(1, 2, 4, 5, 7, 8, 10, 11)) pal_func else pal_func2
      else
        pal <- pal_func
      ecogram_panel(x, xaxes = xaxes,
        right_gap = right_gap, label_cex = label_cex, yfrac_let = yfrac_let, ncols = ncols,
        pal  = pal,
        lab_text = paste(simple_cap(as.character(unique(x$panel))),
          collapse = ""), ...)})
  mtext("Frequency per 100,000 words", side = 2, outer = TRUE, line = -.8,
    col = "grey45", cex = 0.85, las = 0)
}

# gd$panel_lemma <- factor(gd$panel_lemma, levels = c(
#   "conservation",
#   "ecosystem",
#   "population",
#   "diversity",
#   "variability",
#   "distribution",
#   "experiment",
#   "analysis"))

panels_ <- c(

  "species as first",
  "species",
  "conservation",

  "population as first",
  "population",
  "diversity",

  "community as first",
  "community",
  "experiment",

  "ecosystem as first" ,
  "ecosystem",
  "analysis"

  # "variability",
  # "niche",
  # "distribution",

)

gd_ <- filter(gd, panel_lemma %in% panels_)
gd_$panel_lemma <- factor(gd_$panel_lemma, levels = panels_)

gd_$panel_lemma <- forcats::fct_recode(gd_$panel_lemma,
  `species ____` = "species as first",
  `population ____` = "population as first",
  `community ____` = "community as first",
  `ecosystem ____` = "ecosystem as first",
  `____ species` = "species",
  `____ ecosystem` = "ecosystem",
  `____ population` = "population",
  `____ community` = "community",

  `____ conservation` = "conservation",
  `____ diversity` = "diversity",
  `____ experiment` = "experiment",
  `____ analysis` = "analysis"
)

pdf("figs/blanks-viridis5.pdf", width = 7.1, height = 7.0 * 4/3  * gold()*0.97)
gd_ %>%
  plot_blanks(right_gap = 48, log_y = FALSE,
    bottom_frac_up = 0.04, label_gap = -1.0,
    show_seg = TRUE, stop_lab = 0.66)
dev.off()

pairs <- c(
  "species as first",
  "species",
  "population as first",
  "population",
  "community as first",
  "community",
  "ecosystem as first" ,
  "ecosystem"
)

gd2 <- filter(gd, panel_lemma %in% pairs)
gd2$panel_lemma <- factor(gd2$panel_lemma, levels = pairs)

gd2$panel_lemma <- forcats::fct_recode(gd2$panel_lemma,
  `species ____` = "species as first",
  `population ____` = "population as first",
  `community ____` = "community as first",
  `ecosystem ____` = "ecosystem as first",
  `____ species` = "species",
  `____ ecosystem` = "ecosystem",
  `____ population` = "population",
  `____ community` = "community"
)

pal_func <- function(n) {
  viridisLite::plasma(n, begin = 0.0, end = 0.86, direction = -1)
  # viridisLite::magma(n, begin = 0.01, end = 0.85, direction = -1)
  # RColorBrewer::brewer.pal(n, "Dark2")
}

# pdf("figs/blanks-paired.pdf", width = 6.5, height = 6.5 * 4/2  * gold())
# gd2 %>%
#   plot_blanks(right_gap = 23, log_y = FALSE,
#     bottom_frac_up = 0.03, label_gap = -1.0,
#     show_seg = TRUE, pal = pal_func)
# dev.off()

temp <- gd %>%
  filter(panel_lemma %in% c("theory", "theory-of as first", "niche", "niche as first")) %>%
  mutate(panel_lemma = gsub("theory-of as first", "theory of ____", panel_lemma)) %>%
  mutate(panel_lemma = gsub("niche as first", "niche ____", panel_lemma)) %>%
  mutate(panel_lemma = gsub("niche$", "____ Niche", panel_lemma)) %>%
  mutate(panel_lemma = gsub("theory$", "____ Theory", panel_lemma))

temp <- filter(temp, !panel_lemma %in% c("theory of ____", "____ Theory"))
temp$panel_lemma <- factor(temp$panel_lemma,
  levels = c(
    # "theory of ____", "____ Theory",
    "niche ____", "____ Niche"))


pdf("figs/blanks-extras-2018-08-09.pdf", width = 4, height = 7.5 * gold())
plot_blanks(temp, right_gap = 34, log_y = FALSE,
  bottom_frac_up = 0.04, label_gap = -1.0,
  show_seg = TRUE, one_pal = TRUE)
dev.off()


# pdf("figs/blanks2.pdf", width = 6.5, height = 6.5 * 2 * gold())
# gd %>%
#   plot_blanks(right_gap = 34, log_y = FALSE,
#     bottom_frac_up = 0.02, label_gap = -1.0,
#     show_seg = TRUE)
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
