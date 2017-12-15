source("analysis/extract-functions.R")

blank_grams <- ngrams2 %>%
  filter(year %in% c(1940:1949, 2000:2009)) %>%
  filter(gram %like% "% ecology" |
      gram %like% "% experiment" |
      gram %like% "% data" |
      gram %like% "% model" |
      gram %like% "ecological") %>%
  group_by(year, gram) %>%
  summarise(total = sum(count)) %>%
  collect(n = Inf) %>%
  left_join(total1, by = "year") %>%
  ungroup()

save(blank_grams, file = "data/generated/blank_grams.rda")

## fix below 2000 - no 2000 + 1940:

library("koRpus")
set.kRp.env(TT.cmd =
    "~/Dropbox/bin/treetagger/cmd/tree-tagger-english", lang = "en")
tt <- treetag(terms, format = "obj")

x <- blank_grams_2000 %>%
  mutate(panel = stringr::str_split(gram, " ", simplify = TRUE)[,2]) %>%
  mutate(first_word = stringr::str_split(gram, " ", simplify = TRUE)[,1]) %>%
  group_by(panel, gram, first_word) %>%
  summarise(total = sum(total)) %>%
  arrange(panel, -total) %>%
  ungroup() %>%
  group_by(panel) %>%
  top_n(n = 120, wt = total) %>%
  mutate(first_word_type = treetag(first_word, format = "obj")@TT.res$wclass) %>%
  mutate(lemma = treetag(first_word, format = "obj")@TT.res$lemma) %>%
  filter(first_word_type %in% c("adjective", "noun")) %>%
  group_by(panel) %>%
  top_n(n = 40, wt = total)

filter(x, lemma == "<unknown>")
x <- filter(x, !first_word %in% c("ecol"))
x$lemma[x$first_word == "unpubl"] <- "unpublished"
x$lemma[x$lemma == "<unknown>"] <- x$first_word[x$lemma == "<unknown>"]

top_lemmas <- group_by(x, panel, lemma) %>%
  summarise(total = sum(total)) %>%
  group_by(panel) %>%
  top_n(n = 20, wt = total)

terms <- filter(x, lemma %in% top_lemmas$lemma)

gram_dat <- get_ngram_dat(terms$gram)
save(gram_dat, file = "data/generated/top-blank.rda")

gd <- inner_join(gram_dat, rename(terms, total_2000s = total),
  by = "gram")
gd <- group_by(gd, year, panel, lemma, total_words) %>%
  summarise(total = sum(total), total_2000s = sum(total_2000s)) %>%
  ungroup()

top_lemmas_second_cut <- group_by(top_lemmas, panel, lemma) %>%
  summarise(total = sum(total)) %>%
  filter(!lemma %in% c("first", "second")) %>%
  group_by(panel) %>%
  top_n(n = 8, wt = total)

gd <- filter(gd, lemma %in% top_lemmas_second_cut$lemma)



make_panel <- function(dat, lab_dat, title) {
  g <- dat %>%
    filter(year <= 2011, year > 1930) %>%
    ggplot(aes(year, total/total_words*1000, group = lemma)) +
    geom_line(colour = "grey30", alpha = 0.3) +
    geom_col(colour = NA, fill = NA, position = position_dodge()) +
    # facet_wrap(~panel, scales = "free_y") +
    geom_smooth(method = "gam",
      method.args = list(family = gaussian(link = "identity")),
      formula = y ~ s(x), se = FALSE,
      aes(colour = lemma)) +
    ggsidekick::theme_sleek() +
    ggrepel::geom_text_repel(data = lab_dat,
      aes_string(y = "y", label = "lemma", colour = "as.factor(lemma)"),
      size = 4,
      nudge_x = 12,
      segment.size = 0.2,
      segment.color = "#00000030"
    ) +
    scale_x_continuous(breaks = seq(1920, 2012, 20), limits = c(1935, 2038)) +
    guides(colour = FALSE, lty = FALSE) +
    ylab("Instances per 1000 words") + xlab("") +
    ggtitle(title)
}

out <- plyr::dlply(gd, "panel", function(xx) {
  lab <- plyr::ddply(xx, c("panel", "lemma"), function(x) {
    xx <- filter(x, year <= 2011, year > 1930)
    m <- mgcv::gam(total/total_words*1000 ~ s(year), data = xx)
    data.frame(year = 2011, y = predict(m, newdata = data.frame(year = 2011))[[1]])
  })

  make_panel(xx, lab, unique(xx$panel))
})

pdf("figs/blank-panels1.pdf", width = 12, height = 9)
gridExtra::grid.arrange(out[[1]], out[[2]], out[[3]], out[[4]])
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
