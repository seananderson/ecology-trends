source("analysis/extract-functions.R")

bad_latex <- read_csv("data/bad-latex.csv", col_types = cols(gram = col_character()))
excludes <- c("of", "in", "to", "and", "the", "from",
  "fig", "table", "figure", "vol", "tion")
excludes <- union(excludes, bad_latex$gram)

# ----------
# Get the most popular terms in various decades
get_big_grams <- function(gram_db, decades = list(1940:1949, 2000:2009)) {
  pop <- list()
  for (i in seq_along(decades)) {
    message(decades[i])
    pop[[i]] <- gram_db %>% filter(year %in% decades[[i]]) %>%
      filter(!gram %in% excludes) %>%
      # inner_join(total1, by = "year", copy = TRUE) %>%  # from "analysis/extract-functions.R"
      group_by(gram) %>%
      summarise(total = sum(total)) %>%
      arrange(-total) %>%
      collect(n = 5000) %>%
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
tt <- select(tt@TT.res, -desc, -stop, -stem) %>%
  rename(gram = token)
tt <- tt[!duplicated(tt), ]
pop1 <- left_join(pop1, tt, by = "gram")

pop1 <- group_by(pop1, decade) %>%
  filter(tag %in% c("NN", "NNS")) %>%
  # filter(lemma != "<unknown>") %>%
  top_n(n = 500, wt = total) %>%
  ungroup()
pop1$lemma[pop1$lemma == "<unknown>"] <- pop1$gram[pop1$lemma == "<unknown>"]

pop2 <- pop2 %>% filter(!(first_word %in% excludes | second_word %in% excludes)) %>%
  mutate(nchar1 = nchar(first_word), nchar2 = nchar(second_word)) %>%
  filter(nchar1 > 3, nchar2 > 3)

tt_1 <- koRpus::treetag(sort(unique(pop2$first_word)), lang = "en", format = "obj")
tt_1 <- select(tt_1@TT.res, token, wclass, lemma) %>%
  rename(first_word = token, wclass_1 = wclass, lemma_1 = lemma) %>%
  filter(wclass_1 %in% c("adjective", "noun"))
tt_1 <- tt_1[!duplicated(tt_1), ]

tt_2 <- koRpus::treetag(sort(unique(pop2$second_word)), lang = "en", format = "obj")
tt_2 <- select(tt_2@TT.res, token, wclass, lemma) %>%
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
  "corresponding author") %>%
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
pop1 <- pop1 %>% filter(!gram %in% c("fig", "use", "doi", "case"))
pop1 <- pop1 %>% filter(!gram %in% c("tion", "cent", "while", "results"))
pop1 <- pop1 %>% filter(!gram %in% c("table", "figure", "journal"))
pop1 <- pop1 %>% filter(!gram %in% c("university", "author", "eve"))

saveRDS(pop2, file = "data/generated/pop2-cleaned.rds")
saveRDS(pop1, file = "data/generated/pop1-cleaned.rds")

pop2 %>% group_by(decade) %>%
  top_n(n = 10, wt = total) %>%
  select(gram, total, decade) %>% as.data.frame()

# pop1 %>% group_by(decade) %>%
#   top_n(n = 10, wt = total) %>%
#   select(gram, total, decade) %>% as.data.frame()

# ---------
# Extract the relevant time series data and format for plotting
source("analysis/shape_top_decade.R")
pop1_plot <- shape_top_decade(pop1, gram_db = gram_db1, total1 = total1, top = 9)
pop2_plot <- shape_top_decade(pop2, gram_db = gram_db2, total1 = total1, top = 9)


# ----------
# Plot

source("analysis/pretty-panels.R")

plot_decades <- function(dat, right_gap = 30,
  label_cex = 0.85, ...) {
  dat <- dat %>%
    mutate(gram_canonical = lemma, panel = decade) %>%
    arrange(panel, gram_canonical, year)
  npanels <- length(unique(dat$panel))
  nrows <- 1
  ncols <- 2
  par(mfrow = c(nrows, ncols))
  par(mgp = c(2, 0.3, 0), tcl = -0.15, las = 1, cex = 0.7,
    col.axis = "grey55", mar = c(0.025, 2.1, 0, 0), oma = c(1.7, 1.1, .5, .5))
  ii <<- 1
  xaxes <- seq(npanels - (ncols - 1), npanels)
  mutate(dat, total_words = total_words/1e5, total = total) %>%
    plyr::d_ply("panel", function(x) {
      ecogram_panel(x, xaxes = xaxes,
      right_gap = right_gap, label_cex = label_cex, yfrac_let = 0.03,
      lab_text = unique(x$decade), ...)})
  mtext("Frequency per 100,000 words", side = 2, outer = TRUE, line = -0.05,
    col = "grey45", cex = 0.85, las = 0)
}


pdf("figs/decades-1grams.pdf", width = 7, height = 4.5)
pop1_plot %>%
  plot_decades(right_gap = 17, log_y = FALSE,
    bottom_frac_up = 0.01, label_gap = -1.0,
    show_seg = TRUE, ymax = 1200)
dev.off()

pdf("figs/decades-2grams.pdf", width = 7, height = 4.5)
pop2_plot %>%
  plot_decades(right_gap = 36, log_y = FALSE,
    bottom_frac_up = 0.01, label_gap = -1.0,
    show_seg = TRUE, ymax = 45)
dev.off()
