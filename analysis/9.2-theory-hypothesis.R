source("analysis/extract-functions.R")
source("analysis/pretty-panels.R")

load("data/generated/top-blank.rda")
load("data/generated/top-blank3.rda")
load("data/generated/terms-top.rda")
load("data/generated/top-lemmas.rda")

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
  top_n(n = 30, wt = total) %>%
  ungroup()

gd <- inner_join(gd, select(top_lemmas_second_cut, -total),
  by = c("panel_lemma", "lemma"))

gd <- gd %>% mutate(lemma = gsub("specie$", "species", lemma))
gd <- gd %>% mutate(lemma = gsub("datum", "data", lemma))
gd <- gd %>% mutate(panel_lemma = gsub("<unknown> as first", "theory-of as first", panel_lemma))
gd <- gd %>% filter(panel_lemma %in% c("theory-of as first", "theory", "hypothesis"))

top_lemmas_second_cut <- top_lemmas_second_cut %>%
  mutate(panel_lemma = gsub("<unknown> as first", "theory-of as first", panel_lemma))
top_lemmas_second_cut <- top_lemmas_second_cut %>%
  filter(panel_lemma %in% c("theory-of as first", "theory", "hypothesis"))
top_lemmas_second_cut <- top_lemmas_second_cut %>%
  mutate(panel_combined = ifelse(grepl("^theory", panel_lemma), "theory", "hypothesis"))

top_lemmas_second_cut <- filter(top_lemmas_second_cut, !(panel_lemma == "hypothesis" & lemma == "null"))
top_lemmas_second_cut <- filter(top_lemmas_second_cut, !(panel_lemma == "hypothesis" & lemma == "general"))
top_lemmas_second_cut <- filter(top_lemmas_second_cut, !(panel_lemma == "hypothesis" & lemma == "alternative"))
top_lemmas_second_cut <- filter(top_lemmas_second_cut, !(panel_lemma == "hypothesis" & lemma == "third"))
top_lemmas_second_cut <- filter(top_lemmas_second_cut, !(panel_lemma == "theory" & lemma == "ecological"))
top_lemmas_second_cut <- filter(top_lemmas_second_cut, !(panel_lemma == "theory" & lemma == "general"))

top <- group_by(top_lemmas_second_cut, panel_combined) %>%
  top_n(n = 8, wt = total) %>%
  ungroup() %>%
  unique() %>%
  arrange(panel_combined, total)

top <- top %>%
  mutate(
    order = ifelse(panel_combined == "hypothesis", 5, 6),
    panel = ifelse(panel_combined == "hypothesis", "Hypotheses", "Theories"),
    gram = ifelse(grepl("as first", panel_lemma),
      paste("theory of", lemma), paste(lemma, panel_lemma))) %>%
  select(order, panel, gram)

top <- top %>% mutate(gram_canonical = gram)
top <- top %>% mutate(gram_canonical = ifelse(gram_canonical == "history theory", "[life] history theory", gram_canonical))
top <- top %>% mutate(gram_canonical = ifelse(gram_canonical == "theory of island", "theory of island [biogeography]", gram_canonical))
top <- top %>% mutate(gram_canonical = ifelse(gram_canonical == "theory of natural", "theory of natural [selection]", gram_canonical))

top

readr::write_csv(top, "data/ecology_panels_automated.csv")
