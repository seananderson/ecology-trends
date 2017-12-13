source("analysis/extract-functions.R")
library("koRpus")
set.kRp.env(TT.cmd =
    "~/Dropbox/bin/treetagger/cmd/tree-tagger-english", lang = "en")

pop <- list()
decades <- list(1940:1950, 2000:2010)
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

tt <- koRpus::treetag(sort(unique(pop2$gram)), lang = "en", format = "obj")
tt <- select(tt@TT.res, -desc, -stop, -stem) %>%
  rename(gram = token)
pop2 <- left_join(pop2, tt)
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

# # condense if popular in both decades:
# dat4 <- dat4 %>% group_by(year, lemma, grams) %>%
#   summarise(total_words = total_words[1], total = total[1],
#     decade = paste(unique(decade), collapse = " / "))
#
# # make facet label:
# dat4$wrap <- forcats::fct_reorder(paste(dat4$decade, dat4$grams),
#   as.numeric(factor(dat4$decade, levels = c("1940", "1940 / 2010", "2010"))))
#
# g <- dat4 %>% filter(grams != "university") %>%
#   filter(year <= 2011, year > 1930) %>%
#   ggplot(aes(year, total/total_words*10000, group = grams)) +
#   geom_line(colour = "grey50") +
#   geom_col(colour = NA, fill = NA) +
#   facet_wrap(~wrap, scales = "free_y") +
#   geom_smooth(method = "gam",
#     method.args = list(family = gaussian(link = "identity")),
#     formula = y ~ s(x), se = FALSE,
#     aes(colour = as.factor(decade))) +
#   scale_color_manual(values =
#       c("1940 / 2010" = "purple", "1940" = "blue", "2010" = "red")) +
#   ggsidekick::theme_sleek()
#
# ggsave("figs/decades-trial3-bar.pdf", width = 15, height = 12)

dat5 <- dat4 %>% ungroup() %>% mutate(lemma = gsub("specie", "species", lemma))
dat5 <- dat5 %>% ungroup() %>% mutate(lemma = gsub("datum", "data", lemma))
dat5 <- group_by(ungroup(dat5), lemma) %>%
  mutate(list_type = paste(sort(unique(decade)), collapse = " / ")) %>%
  ungroup() %>%
  mutate(list_type2 = grepl(pattern = " / ", list_type)) %>%
  mutate(decade = paste0(decade, "s"))

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
