source("analysis/extract-functions.R")
library("koRpus")
set.kRp.env(TT.cmd =
    "~/Dropbox/bin/treetagger/cmd/tree-tagger-english", lang = "en")

pop <- list()
decades <- list(1940:1949, 2000:2009)
for (i in seq_along(decades)) {
  pop[[i]] <- ngrams1 %>% filter(year %in% decades[[i]]) %>%
    ungroup() %>%
    group_by(gram) %>%
    summarise(total = sum(count)) %>%
    arrange(-total) %>%
    collect(n = 10000) %>%
    filter(!grepl("[0-9]+", gram)) %>%
    filter(nchar(gram) >= 3)
  pop[[i]]$decade <- min(decades[[i]])
}
save(pop, file = "data/generated/pop.rda")
load("data/generated/pop.rda")
pop2 <- bind_rows(pop)

decades <- list(1940:1949, 1970:1979, 2000:2009)
pop_2grams <- list()
for (i in seq_along(decades)) {
  pop_2grams[[i]] <- ngrams2 %>% filter(year %in% decades[[i]]) %>%
    ungroup() %>%
    group_by(gram) %>%
    summarise(total = sum(count)) %>%
    arrange(-total) %>%
    collect(n = 10000) %>%
    filter(!grepl("[0-9]+", gram)) %>%
    filter(nchar(gram) >= 3)
  pop_2grams[[i]]$decade <- min(decades[[i]])
}
save(pop_2grams, file = "data/generated/pop_2grams.rda")
load("data/generated/pop_2grams.rda")
pop_2grams <- bind_rows(pop_2grams)

tt <- koRpus::treetag(sort(unique(pop2$gram)), lang = "en", format = "obj")
tt <- select(tt@TT.res, -desc, -stop, -stem) %>%
  rename(gram = token)
tt <- tt[!duplicated(tt), ]
pop2 <- left_join(pop2, tt)

pop_2grams <- pop_2grams %>%
  mutate(first_word = stringr::str_split(gram, " ", simplify = TRUE)[,1]) %>%
  mutate(second_word = stringr::str_split(gram, " ", simplify = TRUE)[,2]) %>%
  filter(nchar(first_word) >= 3, nchar(second_word) >= 3) %>%
  filter(!first_word %in% c("the", "and", "for"),
    !second_word %in% c("the", "and", "for"))

tt_1 <- koRpus::treetag(sort(unique(pop_2grams$first_word)), lang = "en", format = "obj")
tt_1 <- select(tt_1@TT.res, token, wclass, lemma) %>%
  rename(first_word = token, wclass_1 = wclass, lemma_1 = lemma) %>%
  filter(wclass_1 %in% c("adjective", "noun"))
tt_1 <- tt_1[!duplicated(tt_1), ]

tt_2 <- koRpus::treetag(sort(unique(pop_2grams$second_word)), lang = "en", format = "obj")
tt_2 <- select(tt_2@TT.res, token, wclass, lemma) %>%
  rename(second_word = token, wclass_2 = wclass, lemma_2 = lemma) %>%
  filter(wclass_2 %in% c("adjective", "noun"))
tt_2 <- tt_2[!duplicated(tt_2), ]

pop_2grams <- inner_join(pop_2grams, tt_1, by = "first_word") %>%
  inner_join(tt_2, by = "second_word")
pop_2grams <- pop_2grams %>% arrange(decade, total)
exclude <- c("fig", "table", "figure", "vol", "tion")
pop_2grams <- pop_2grams %>% filter(!first_word %in% exclude,
  !second_word %in% exclude)
readr::write_csv(pop_2grams, "data/decade-top-2grams.csv")
# pop_2grams <- read_csv("data/decade-top-2grams.csv")

# d2 <- feather::read_feather("data/generated/condensed-ngrams2.feather")
# qq <- filter(d2, gram %in% unique(pop_2grams$gram))

#######################

pop_write <- group_by(pop2, decade) %>%
  filter(tag %in% c("NN", "NNS")) %>%
  filter(lemma != "<unknown>") %>%
  top_n(n = 2000, wt = total) %>%
  ungroup()
pop_write <- pop_write[!duplicated(pop_write), ]
readr::write_csv(pop_write, "data/decade-top-2000.csv")


# -------------------------------------------
pop3 <- group_by(pop2, decade) %>%
  filter(tag %in% c("NN", "NNS")) %>%
  filter(lemma != "<unknown>") %>%
  top_n(n = 50, wt = total) %>%
  ungroup()

pop3 <- pop3[!duplicated(pop3), ]
filter(pop3, lttr == 3)
pop3 <- pop3 %>% filter(!gram %in% c("fig", "use"))
pop3 <- pop3 %>% filter(!gram %in% c("table", "figure", "journal"))
filter(pop3, lttr == 3)
filter(pop3, lttr == 4) %>% as.data.frame()
pop3 <- pop3 %>% filter(!gram %in% c("tion", "cent", "while", "results"))
filter(pop3, lttr == 5) %>% as.data.frame()
filter(pop3, lttr == 6) %>% as.data.frame()
filter(pop3, lttr > 6) %>% as.data.frame()
pop3 <- pop3 %>% filter(!gram %in% c("university"))

keep <- group_by(pop3, decade, lemma) %>%
  top_n(n = 1, wt = total) %>%
  ungroup() %>%
  group_by(decade) %>%
  top_n(n = 10, wt = total) %>%
  ungroup() %>%
  select(decade, lemma) %>%
  as.data.frame()

pop3_keep <- inner_join(pop3, keep, by = c("decade", "lemma"))

# extract:
dat3 <- ngrams1 %>% filter(gram %in% pop3_keep$gram) %>%
  group_by(year, gram) %>%
  summarise(total = sum(count)) %>%
  collect(n = Inf) %>%
  left_join(total1, by = "year") %>%
  ungroup()
saveRDS(dat3, file = "data/generated/decade-evolution-dat.rds")
dat3 <- readRDS("data/generated/decade-evolution-dat.rds")

# add back lemma column:
dat4 <- inner_join(dat3, select(pop3_keep, lemma, gram), by = "gram")

# condense across lemmas:
condensed_across_lemmas <- ungroup(dat4) %>%
  group_by(year, total_words, lemma) %>%
  summarize(total = sum(total),
    grams = paste(sort(unique(gram)), collapse = ", "))

# add decade back:
dat4 <- full_join(condensed_across_lemmas,
  unique(select(pop3_keep, lemma, decade)), by = "lemma")

dat5 <- dat4 %>% ungroup() %>% mutate(lemma = gsub("specie", "species", lemma))
dat5 <- dat5 %>% ungroup() %>% mutate(lemma = gsub("datum", "data", lemma))
dat5 <- group_by(ungroup(dat5), lemma) %>%
  mutate(list_type = paste(sort(unique(decade)), collapse = " / ")) %>%
  ungroup() %>%
  mutate(list_type2 = grepl(pattern = " / ", list_type)) %>%
  mutate(decade = paste0(decade, "s"))

# plot
# ---------------------
lab <- plyr::ddply(dat5, c("decade", "lemma", "list_type"), function(x) {
  xx <- filter(x, year <= 2011, year > 1930)
  m <- mgcv::gam(total/total_words*1000 ~ s(year), data = xx)
  data.frame(year = 2011, y = predict(m, newdata = data.frame(year = 2011))[[1]])
})

g <- dat5 %>% filter(grams != "university") %>%
  filter(year <= 2011, year > 1930) %>%
  ggplot(aes(year, total/total_words*1000, group = lemma)) +
  geom_line(colour = "grey30", alpha = 0.3) +
  geom_col(colour = NA, fill = NA, position = position_dodge()) +
  facet_wrap(~decade, scales = "fixed") +
  geom_smooth(method = "gam",
    method.args = list(family = gaussian(link = "identity")),
    formula = y ~ s(x), se = FALSE,
    aes(colour = as.factor(lemma), lty = list_type2)) +
  # scale_color_manual(values =
      # c("1940 / 2000" = "purple", "1940" = "blue", "2000" = "red")) +
  ggsidekick::theme_sleek() +
  ggrepel::geom_text_repel(data = lab,
    aes_string(y = "y", label = "lemma", colour = "as.factor(lemma)"),
    size = 4,
    nudge_x = 10,
    segment.size = 0.2,
    segment.color = "#00000030"
  ) +
  scale_x_continuous(breaks = seq(1920, 2012, 20), limits = c(1935, 2038)) +
  guides(colour = FALSE, lty = FALSE) +
  ylab("Instances per 1000 words") + xlab("")

ggsave("figs/decades-trial5.pdf", width = 9, height = 6.5)
