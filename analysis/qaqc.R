# library(magick)
# pdf <- image_read("~/Desktop/1930126.pdf", density = "144x144")
# samples_per_page <- 10L
# pdf("~/Desktop/test-out.pdf", width = 8, height = 10)
# for (i in seq(2, length(pdf))) {
# info <- image_info(pdf[i])
# width <- info$width
# height <- info$height
# border <- width / 6
# par(mar = c(0, 0, 0, 0), oma = c(0, 0, 0, 0))
# plot(as.raster(pdf[i]))
# points(
#   x = runif(samples_per_page, 0 + border, width - border),
#   y = runif(samples_per_page, 0 + border, height - border),
#   col = "red", pch = 21, cex = 2)
# }
# dev.off()

# --------

source("analysis/extract-functions.R")

total1_by_journal <- ngrams1 %>%
  group_by(year, journal) %>%
  summarise(total_words = sum(count)) %>%
  collect(n = Inf)
total1_by_journal <- rename(total1_by_journal,
  total_words_journal = total_words)

terms <- c("ecology", "because", "then", "water", "large",
  "animals", "plants", "experiments", "which", "where", "were")

dat <- ngrams1 %>% filter(gram %in% terms) %>%
  group_by(year, gram, journal) %>%
  summarise(total = sum(count)) %>%
  collect(n = Inf) %>%
  left_join(total1)

terms2 <- c("biology", "thus", "therefore", "male", "female", "observed")
dat2 <- ngrams1 %>% filter(gram %in% terms2) %>%
  group_by(year, gram, journal) %>%
  summarise(total = sum(count)) %>%
  collect(n = Inf) %>%
  left_join(total1)

dat3 <- bind_rows(dat, dat2)

dat3 <- left_join(dat3, total1_by_journal)

test_all <- group_by(dat3, year, gram) %>%
  summarise(total = sum(total), total_words = total_words[[1]])

test_all %>%
  filter(year <= 2011, year > 1930) %>%
  ggplot(aes(year, total/total_words*10000)) +
  geom_line(colour = "grey50") +
  facet_wrap(~gram, scales = "free_y") +
  geom_smooth(colour = "red", method = "gam",
    method.args = list(family = gaussian(link = "identity")),
    formula = y ~ s(x), se = FALSE)
ggsave("figs/generic-word-test.pdf", width = 9, height = 9)

dat3 <- group_by(dat3, journal) %>%
  mutate(n_journal = sum(total_words_journal)) %>%
  ungroup() %>%
  group_by(gram) %>%
  mutate(n_gram = mean(total/total_words_journal)) %>%
  ungroup()

g <- dat3 %>%
  filter(year <= 2011, year > 1930, n_journal > 88344988) %>%
  ggplot(aes(year, total/total_words_journal*10000)) +
  geom_line(alpha = 0.9, aes(group = journal)) +
  facet_grid(
    forcats::fct_reorder(gram, -n_gram)~
      forcats::fct_reorder(journal, -n_journal),
    scales = "free_y") +
  guides(colour = FALSE)
# geom_smooth(colour = "red", method = "gam",
# method.args = list(family = gaussian(link = "identity")),
# formula = y ~ s(x), se = FALSE)
# g
ggsave("figs/generic-word-test-journal.pdf", width = 35, height = 17)

# --- pop. words by decade?

pop <- list()
decades <- seq(1940, 2010, 10)
decades <- c(1940, 2010)
for (i in seq_along(decades)) {
  pop[[i]] <- ngrams1 %>% filter(year %in% decades[i]) %>%
    group_by(year, gram) %>%
    summarise(total = sum(count)) %>%
    arrange(-total) %>%
    collect(n = 4000) %>%
    filter(!grepl("[0-9]+", gram)) %>%
    filter(nchar(gram) >= 3)
}
# temp:
pop[[1]]$year <- 1940
pop[[2]]$year <- 2010
# pop <- ngrams1 %>% filter(year %in% 1940) %>%
#   group_by(year, gram) %>%
#   summarise(total = sum(count)) %>%
#   arrange(-total) %>%
#   collect(n = 1000)

save(pop, file = "data/generated/pop.rda")

pop_df <- bind_rows(pop)
pop2 <- ungroup(pop_df) %>%
  group_by(year) %>%
  top_n(n = 150, wt = total) %>%
  ungroup()

readr::write_csv(pop2, path = "data/generated/pop.csv")

pop2 <- readr::read_csv("data/pop.csv")
pop2 <- filter(pop2, eco == 1)

pop3 <- group_by(pop2, year) %>%
  top_n(n = 25, wt = total) %>%
  ungroup()

dat3 <- ngrams1 %>% filter(gram %in% pop3$gram) %>%
  group_by(year, gram) %>%
  summarise(total = sum(count)) %>%
  collect(n = Inf) %>%
  left_join(total1)

dat4 <- full_join(dat3, rename(select(pop3, -total), decade = year))

# dat4_1940 <- inner_join(dat3, filter(rename(select(pop3, -total), decade = year), decade == 1940))
# dat4_2010 <- inner_join(dat3, filter(rename(select(pop3, -total), decade = year), decade == 2010))
#
# dat4 <- bind_rows(list(dat4_1940, dat4_2010))

dat4 <- dat4 %>% group_by(year, gram) %>%
  summarise(total_words = total_words[1], total = total[1],
    decade = paste(unique(decade), collapse = " / "))

g <- dat4 %>%
  filter(year <= 2011, year > 1930) %>%
  ggplot(aes(year, total/total_words*10000, group = gram)) +
  geom_line(colour = "grey50") +
  facet_wrap(~decade, scales = "free_y") +
  geom_smooth(method = "gam",
    method.args = list(family = gaussian(link = "identity")),
    formula = y ~ s(x), se = FALSE, aes(colour = as.factor(decade)))

ggsave("figs/decades-trial1.pdf", width = 9, height = 6)

dat4$wrap <- fct_reorder(paste(dat4$decade, dat4$gram), as.numeric(factor(dat4$decade, levels = c("1940", "1940 / 2010", "2010"))))

g <- dat4 %>%
  filter(year <= 2011, year > 1930) %>%
  ggplot(aes(year, total/total_words*10000, group = gram)) +
  geom_line(colour = "grey50") +
  facet_wrap(~wrap, scales = "free_y") +
  geom_smooth(method = "gam",
    method.args = list(family = gaussian(link = "identity")),
    formula = y ~ s(x), se = FALSE, aes(colour = as.factor(decade))) +
  scale_color_manual(values = c("1940 / 2010" = "purple", "1940" = "blue", "2010" = "red"))

ggsave("figs/decades-trial2.pdf", width = 15, height = 12)
