source("analysis/extract-functions.R")
source("analysis/booming.R")
source("analysis/pretty-panels.R")
source("analysis/frontiers_theme.R")
total1 <- readRDS(here::here("data/generated/total1.rds"))
g <- total1 %>%
  filter(year >= 1930, year <= 2010) %>%
  ggplot(aes(year, total_words / 1e6)) +
  geom_point() +
  geom_line() +
  frontiers_theme() +
  ylab("Millions of 1-grams") + xlab("Year")
ggsave("figs/webfigure2.pdf", width = 5.5, height = 0.618 * 5.5)

bad_latex <- read_csv("data/bad-latex.csv", col_types = cols(gram = col_character()))
excludes1 <- c("of", "in", "to", "and", "the", "from",
  "fig", "table", "figure", "vol", "tion")
excludes <- union(excludes1, bad_latex$gram)

# ----------
# Get the most popular terms in various decades
get_big_grams <- function(gram_db, decades = list(1940:1949, 2000:2009)) {
  pop <- list()
  for (i in seq_along(decades)) {
    message(decades[i])
    pop[[i]] <- gram_db %>% filter(year %in% !!decades[[i]]) %>%
      filter(!gram %in% excludes) %>%
      # inner_join(total1, by = "year", copy = TRUE) %>%  # from "analysis/extract-functions.R"
      group_by(gram) %>%
      summarise(total = sum(total)) %>%
      arrange(-total) %>%
      collect(n = 7000) %>%
      filter(!grepl("[0-9]+", gram)) %>%
      filter(nchar(gram) >= 3)
    pop[[i]]$decade <- min(decades[[i]])
  }
  bind_rows(pop)
}

pop1 <- get_big_grams(gram_db1)
pop2 <- get_big_grams(gram_db2)

saveRDS(pop1, file = "data/generated/pop1.rds")
saveRDS(pop2, file = "data/generated/pop2.rds")
pop1 <- readRDS("data/generated/pop1.rds")
pop2 <- readRDS("data/generated/pop2.rds")

pop2 <- pop2 %>%
  mutate(second_word = stringr::str_split(gram, " ", simplify = TRUE)[,2]) %>%
  mutate(first_word = stringr::str_split(gram, " ", simplify = TRUE)[,1])

# ----------
# Keep just the nouns and adjectives
tt <- koRpus::treetag(sort(unique(pop1$gram)), lang = "en", format = "obj")
tt <- select(tt@tokens, -desc, -stop, -stem) %>%
  rename(gram = token)
tt <- tt[!duplicated(tt), ]
pop1 <- left_join(pop1, tt, by = "gram")

pop1 <- group_by(pop1, decade) %>%
  filter(tag %in% c("NN", "NNS")) %>%
  # filter(lemma != "<unknown>") %>%
  top_n(n = 800, wt = total) %>%
  ungroup()
pop1$lemma[pop1$lemma == "<unknown>"] <- pop1$gram[pop1$lemma == "<unknown>"]

pop2 <- pop2 %>% filter(!(first_word %in% excludes | second_word %in% excludes)) %>%
  mutate(nchar1 = nchar(first_word), nchar2 = nchar(second_word)) %>%
  filter(nchar1 > 3, nchar2 > 3)

tt_1 <- koRpus::treetag(sort(unique(pop2$first_word)), lang = "en", format = "obj")
tt_1 <- select(tt_1@tokens, token, wclass, lemma) %>%
  rename(first_word = token, wclass_1 = wclass, lemma_1 = lemma) %>%
  filter(wclass_1 %in% c("adjective", "noun"))
tt_1 <- tt_1[!duplicated(tt_1), ]

tt_2 <- koRpus::treetag(sort(unique(pop2$second_word)), lang = "en", format = "obj")
tt_2 <- select(tt_2@tokens, token, wclass, lemma) %>%
  rename(second_word = token, wclass_2 = wclass, lemma_2 = lemma) %>%
  filter(wclass_2 %in% c("adjective", "noun"))
tt_2 <- tt_2[!duplicated(tt_2), ]

pop2 <- inner_join(pop2, tt_1, by = "first_word") %>%
  inner_join(tt_2, by = "second_word")

# ----------
# Clean out words we aren't interested in

exclude <- c("american naturalist", "ecological monographs",
  "biol bull", "woods hole", "experi ments",
  "condi tions", "indi viduals", "comp physiol",
  "empty declaremathsizes", "university press",
  "conservation biology", "ecological society",
  "coastal research", "Journal compilation",
  "Blackwell publishing", "british ecological",
  "ecological applications", "cambridge university",
  "authors journal", "canadian journal",
  "royal society", "ecol evol", "state university",
  "springer verlag", "phil trans", "much more", "same time",
  "united states", "other hand", "philosophical transactions",
  "corresponding editor", "ecol monogr", "conserv biol",
  "ecol lett", "popu lations", "ecology letters",
  "research institute", "press chicago", "ecol syst",
  "funct ecol", "change biology", "author contributions",
  "phil wrans", "portugal issn", "nati acad", "proc nati",
  "corresponding author", "other species", "document documentclass") %>%
  tolower()
pop2 <- pop2 %>% filter(!first_word %in% exclude,
  !second_word %in% exclude, !gram %in% exclude)

pop2$lemma_1[pop2$lemma_1 == "<unknown>"] <- pop2$first_word[pop2$lemma_1 == "<unknown>"]
pop2$lemma_2[pop2$lemma_2 == "<unknown>"] <- pop2$second_word[pop2$lemma_2 == "<unknown>"]
pop2 <- mutate(pop2, lemma = paste(lemma_1, lemma_2))

pop2 <- pop2 %>%
  group_by(decade) %>%
  top_n(n = 500, wt = total) %>%
  ungroup() %>%
  arrange(decade, -total)

# pop1 %>% as.data.frame()
exclude3 <- c("fig", "use", "doi", "case",
  "tion", "cent", "while", "results",
  "table", "figure", "journal",
  "university", "author", "eve")
pop1 <- pop1 %>% filter(!gram %in% exclude3)

saveRDS(pop2, file = "data/generated/pop2-cleaned.rds")
saveRDS(pop1, file = "data/generated/pop1-cleaned.rds")
pop1 <- readRDS("data/generated/pop1-cleaned.rds")
pop2 <- readRDS("data/generated/pop2-cleaned.rds")

pop2 %>% group_by(decade) %>%
  top_n(n = 14, wt = total) %>%
  select(gram, total, decade) %>% as.data.frame()

# ---------
# Extract the relevant time series data and format for plotting
source("analysis/shape_top_decade.R")
pop1_plot <- shape_top_decade(pop1, gram_db = gram_db1, total1 = total1,
  top = 9)
pop2_plot <- shape_top_decade(pop2, gram_db = gram_db2, total1 = total1,
  top = 9)

# ----------
# Plot

plot_decades <- function(dat, right_gap = 30,
  label_cex = 0.85, nrows = 2, ncols = 2, yfrac_let = 0.04, ...) {
  dat <- dat %>%
    mutate(gram_canonical = lemma, panel = paste(decade, gram_num)) %>%
    arrange(panel, gram_canonical, year) %>%
    mutate(panel = forcats::fct_relevel(panel,
      "1940s 1-grams", "2000s 1-grams", "1940s 2-grams", "2000s 2-grams"))
  npanels <- length(unique(dat$panel))
  par(mfrow = c(nrows, ncols))
  par(mgp = c(2, 0.3, 0), tcl = -0.15, las = 1, cex = 0.7,
    col.axis = "grey05", mar = c(0.025, 2.1, 0, 0), oma = c(1.7, 1.1, .5, .5))
  ii <<- 1
  xaxes <- seq(npanels - (ncols - 1), npanels)
  mutate(dat, total_words = total_words/1e5, total = total) %>%
    plyr::d_ply("panel", function(x) {
      ecogram_panel(x, xaxes = xaxes,
      right_gap = right_gap, label_cex = label_cex, yfrac_let = yfrac_let,
      lab_text = unique(x$panel), ymax = unique(x$ymax), ...)})
  mtext("Frequency per 100,000 words", side = 2, outer = TRUE, line = -0.05,
    col = "grey05", cex = 0.85, las = 0)
}

df <- tibble(decade = c("1940s", "1940s", "2000s", "2000s"),
  gram_num = c("1-grams", "2-grams", "1-grams", "2-grams"),
  ymax = c(1200, 50, 1200, 50))

# pdf("figs/decades.pdf", width = 7, height = 5.9)
# pop1_plot %>% mutate(gram_num = "1-grams") %>%
#   bind_rows(mutate(pop2_plot, gram_num = "2-grams")) %>%
#   inner_join(df, by = c("decade", "gram_num")) %>%
#   plot_decades(right_gap = 36, log_y = FALSE,
#     bottom_frac_up = 0.015, label_gap = -1.0,
#     show_seg = TRUE)
# dev.off()

# booming -----------------------------------------------------------
pop1 <- readRDS("data/generated/pop1-cleaned.rds")
pop2 <- readRDS("data/generated/pop2-cleaned.rds")
pop2 <- filter(pop2, !gram %in% c("ecol appl"))
pop2 <- filter(pop2, !gram %in% c("chromo somes"))
pop1 <- filter(pop1, !gram %in% c("author"))
pop1 <- filter(pop1, !gram %in% c("authors"))
pop1 <- filter(pop1, !gram %in% c("writer"))
p1 <- shape_top_decade(pop1, gram_db = gram_db1, total1 = total1, top = 300)
p2 <- shape_top_decade(pop2, gram_db = gram_db2, total1 = total1, top = 300)
b1 <- process_slopes(p1)
b2 <- process_slopes(p2)
bb <- bind_rows(
  mutate(b1, decade = paste(decade, "1-grams")),
  mutate(b2, decade = paste(decade, "2-grams")))

dec_dat <- pop1_plot %>% mutate(gram_num = "1-grams") %>%
  bind_rows(mutate(pop2_plot, gram_num = "2-grams")) %>%
  inner_join(df, by = c("decade", "gram_num")) %>%
  mutate(decade = paste(decade, gram_num))

boom_dat <- filter(bb, lemma != "") %>%
  filter(decade %in% c("2000s 1-grams", "2000s 2-grams")) %>%
  mutate(decade = paste(decade, "increase"))

both_dat <- bind_rows(dec_dat, boom_dat) %>%
  mutate(decade = factor(decade,
    levels = c(
      "1940s 1-grams",
      "1940s 2-grams",
      "2000s 1-grams",
      "2000s 2-grams",
      "2000s 1-grams increase",
      "2000s 2-grams increase"
    )))

both_dat$lemma[both_dat$lemma=="supplementary material"] <- "supplementary\nmaterial"

pdf("figs/decades-and-booms.pdf", width = 5.25,
  height = 5 * gold() * 3/2 * 0.96)
par(lheight = 0.6)
plot_decades_and_boom(both_dat, right_gap = 70, nrows = 3, ncols = 2)
dev.off()

pdf("figs/decades-and-booms-viridis.pdf", width = 5.25,
  height = 5 * gold() * 3/2 * 0.96)
par(lheight = 0.6)
plot_decades_and_boom(both_dat, right_gap = 70, nrows = 3, ncols = 2,
  pal = pal_func)
dev.off()

boom_only_dat <- filter(bb, lemma != "")
  # filter(decade %in% c("1940s 1-grams", "1940s 2-grams")) %>%
  # mutate(decade = paste(decade, "decrease"))

pdf("figs/booms-viridis.pdf", width = 7,
  height = 7 * gold() * 1 * 1.1)
plot_boom(boom_only_dat, right_gap = 50, nrows = 2, ncols = 2,
  pal = pal_func)
dev.off()

pdf("figs/booms.pdf", width = 7,
  height = 7 * gold() * 1 * 1.1)
plot_boom(boom_only_dat, right_gap = 50, nrows = 2, ncols = 2)
dev.off()

pdf("figs/booms-viridis-decline.pdf", width = 7,
  height = .5 * 7 * gold() * 1 * 1.1)
plot_boom(boom_only_dat, right_gap = 50, nrows = 1, ncols = 2,
  pal = pal_func)
dev.off()

pdf("figs/booms-decline.pdf", width = 7,
  height = .5 * 7 * gold() * 1 * 1.1)
plot_boom(filter(boom_only_dat, list_type == "1940"), right_gap = 50, nrows = 1, ncols = 2)
dev.off()

exclusions_fig1 <- sort(unique(c(excludes1, exclude, exclude3)))
saveRDS(exclusions_fig1, file = here::here("data/generated/exclusions_fig1.rds"))
