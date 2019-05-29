source("analysis/extract-functions.R")
# source("analysis/booming.R")
source("analysis/pretty-panels.R")

bad_latex <- read_csv("data/bad-latex.csv", col_types = cols(gram = col_character()))
excludes <- c("of", "in", "to", "and", "the", "from",
  "fig", "table", "figure", "vol", "tion")
exclude2 <- c("fig", "use", "doi", "case", "tion", "cent", "while", "results", "table", "figure", "journal", "university", "author", "eve")
exclude3 <- c("american naturalist", "ecological monographs",
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
  "corresponding author")
excludes <- union(excludes, bad_latex$gram)
excludes <- union(excludes, exclude2)
excludes <- union(excludes, exclude3)

# ----------
# Get the most popular terms in various decades
get_big_grams <- function(gram_db,
  decades = list(1920:1959, 1960:1979, 1980:1999, 2000:2010)) {
  pop <- list()
  for (i in seq_along(decades)) {
    message(decades[i])
    pop[[i]] <- gram_db %>% filter(year %in% decades[[i]]) %>%
      filter(!gram %in% excludes) %>%
      group_by(gram) %>%
      summarise(total = sum(total), n_obs = n()) %>%
      filter(n_obs >= 10) %>%
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

saveRDS(pop1, file = "data/generated/pop1-giant.rds")
saveRDS(pop2, file = "data/generated/pop2-giant.rds")
pop1 <- readRDS("data/generated/pop1-giant.rds")
pop2 <- readRDS("data/generated/pop2-giant.rds")

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
  top_n(n = 2000, wt = total) %>%
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

pop2$lemma_1[pop2$lemma_1 == "<unknown>"] <- pop2$first_word[pop2$lemma_1 == "<unknown>"]
pop2$lemma_2[pop2$lemma_2 == "<unknown>"] <- pop2$second_word[pop2$lemma_2 == "<unknown>"]
pop2 <- mutate(pop2, lemma = paste(lemma_1, lemma_2))

pop2 <- pop2 %>%
  group_by(decade) %>%
  top_n(n = 500, wt = total) %>%
  ungroup() %>%
  arrange(decade, -total)

saveRDS(pop1, file = "data/generated/pop1-cleaned-giant.rds")
saveRDS(pop2, file = "data/generated/pop2-cleaned-giant.rds")
pop1 <- readRDS("data/generated/pop1-cleaned-giant.rds")
pop2 <- readRDS("data/generated/pop2-cleaned-giant.rds")

# pop2 %>% group_by(decade) %>%
#   top_n(n = 10, wt = total) %>%
#   select(gram, total, decade) %>% as.data.frame()

# ---------
# Extract the relevant time series data and format for plotting
source("analysis/shape_top_decade.R")

exclude <- c("iii", "nat", "min", "bull", "physiol", "zool", "line", "und", "note", "trans", "ment", "pre", "dis", "way", "pre", "soc", "other", "i.e", "ing", "biol",
  "might", "ecol", "fontenc", "encodingdefault", "American", "part", "well", "following", "paper", "example", "term", "show", "press")
pop1 <- pop1 %>% filter(!gram %in% exclude)

exclude2 <- c("nature lond", "tempera ture", "popula tion", "tempera tures", "cell comp", "raqhi nala", "much high", "limnol oceanogr", "anim ecol", "plant physiol", "biochem physiol", "abun dance", "natl acad", "progress series", "trend ecol", "blackwell publishing", "annual review", "factor such", "national academy", "behav ecol", "academi press", "comment ecology", "comp biochem", "address department", "science foundation", "geol surv", "press princeton", "john wiley", "signifi cantly", "popu lation", "proc natl", "academic press", "research council", "document documentclass")
pop2 <- pop2 %>% filter(!lemma %in% exclude2)

# pop2_plot <- pop2 %>% group_by(decade, lemma) %>%
#   mutate(n_years_with_data = n()) %>%
#   filter(n_years_with_data > 5) %>%
#   ungroup()

pop1_plot <- shape_top_decade(pop1, gram_db = gram_db1, total1 = total1,
  top = 144)
pop2_plot <- shape_top_decade(pop2, gram_db = gram_db2, total1 = total1, top = 144)

# ----------
# Plot

metadata <- data.frame(decade = c("1920s", "1960s", "1980s", "2000s"),
  decade_minimum = c(1920, 1960, 1980, 2000),
  decade_maximum = c(1959, 1979, 1999, 2010), stringsAsFactors = FALSE)

keep <- pop1_plot %>% inner_join(metadata, by = "decade") %>%
  group_by(decade, lemma) %>%
  filter(year >= decade_minimum, year <= decade_maximum) %>%
  summarise(total = sum(total)) %>%
  ungroup() %>%
  mutate(lemma_total = total) %>%
  select(decade, lemma, lemma_total)

pop1_keep <- inner_join(pop1_plot, keep, by = c("decade", "lemma"))

keep2 <- pop2_plot %>% inner_join(metadata, by = "decade") %>%
  group_by(decade, lemma) %>%
  filter(year >= decade_minimum, year <= decade_maximum) %>%
  summarise(total = sum(total)) %>%
  ungroup() %>%
  mutate(lemma_total = total) %>%
  select(decade, lemma, lemma_total)

pop2_keep <- inner_join(pop2_plot, keep2, by = c("decade", "lemma"))

make_giant_decadal_figure <- function(data_, decade_begin, decade_end, type = "1gram") {
  .decade <- paste0(decade_begin, "s")
  filename <- paste0(decade_begin, "-", decade_end, "-", type)
  data_$lemma <- tolower(data_$lemma)
  data_ %>%
    filter(decade == .decade) %>%
    ggplot(aes(year, total / total_words * 100000)) +
    ggsidekick::theme_sleek() +
    geom_rect(
      xmin = decade_begin, xmax = decade_end, ymin = 0, ymax = 3000,
      colour = "grey85", fill = "grey85") +
    # geom_point(size = 1, pch = 21) +
    geom_line() +
    geom_smooth(colour = "red") +
    facet_wrap(~forcats::fct_reorder(lemma, -lemma_total),
      scales = "free_y", ncol = 9) +
    ylab("Frequency per 100,000 words") +
    xlab("Year")
  ggsave(paste0("figs/decadal-giant-", filename, ".pdf"), width = 13, height = 17)
}

make_giant_decadal_figure(pop1_keep, 1920, 1959)
make_giant_decadal_figure(pop1_keep, 1960, 1979)
make_giant_decadal_figure(pop1_keep, 1980, 1999)
make_giant_decadal_figure(pop1_keep, 2000, 2010)

make_giant_decadal_figure(pop2_keep, 1920, 1959, type = "2gram")
make_giant_decadal_figure(pop2_keep, 1960, 1979, type = "2gram")
make_giant_decadal_figure(pop2_keep, 1980, 1999, type = "2gram")
make_giant_decadal_figure(pop2_keep, 2000, 2010, type = "2gram")
