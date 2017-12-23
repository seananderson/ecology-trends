source("analysis/extract-functions.R")

plot_group_clown_vomit <- function(dat, group_name, width = 30, height = 25) {
  dd <- filter(dat, group %in% group_name, year <= 2012, year >= 1935) %>%
    ungroup() %>%
    group_by(gram) %>%
    mutate(max_count = max(total/total_words))

  dd <- group_by(dd, gram) %>%
    filter(total > 10) %>%
    mutate(n_year = n()) %>%
    ungroup() %>%
    filter(n_year >= 5)

  library(mgcv)

  dd2 <- plyr::ddply(dd, "gram", function(x) {
    m <- tryCatch({gam(I(log10(total/total_words*10000)) ~ s(year), data = x)},
      error = function(ignore) gam(I(log10(total/total_words*10000)) ~ s(year, k = 3), data = x)
    )
    mlm <- lm(I(log10(total/total_words*10000)) ~ year, data = x)
    y <- seq(min(x$year), max(x$year), 0.5)
    data.frame(year = y, pred = predict(m, newdata = data.frame(year = y)),
      slope = coef(mlm)[[2]])
  })

  labs_min <- group_by(dd2, gram) %>%
    summarise(pred = pred[year == min(year)],
      slope = slope[year == min(year)], year = min(year))
  labs_max <- group_by(dd2, gram) %>%
    summarise(pred = pred[year == max(year)],
      slope = slope[year == max(year)], year = max(year))

  g <- ggplot(dd2, aes(year, pred, group = gram, colour = gram)) +
    theme_sleek() +
    geom_line(lwd = 0.8) +
    ggrepel::geom_text_repel(
      data = labs_min,
      aes(label = gram),
      size = 3,
      nudge_x = -5,
      segment.size = 0.2,
      segment.color = "#00000030"
    ) +
    ggrepel::geom_text_repel(
      data = labs_max,
      aes(label = gram),
      size = 3,
      nudge_x = 10,
      segment.size = 0.2,
      segment.color = "#00000030"
    ) +
    ylab("log10(Instances per 10000 words)") +
    ggtitle(group_name) +
    xlim(1925, 2025) +
    scale_colour_discrete(guide = 'none') +
    # scale_color_gradient2(
    # low = scales::muted("blue"),
    # mid = "grey60",
    # high = scales::muted("red"), guide = "none") +
    scale_alpha_continuous(limits = c(-20, -2))
  # viridis::scale_colour_viridis(discrete = TRUE)
  ggsave(paste0("figs/grouped-", group_name, ".pdf"), width = width, height = height)
}

##############

terms <- read_csv("data/Group2_TermRequest1.csv")
dat <- get_ngram_dat(terms$terms)
saveRDS(dat, file = "data/generated/group2-request1.rds")
dat <- readRDS("data/generated/group2-request1.rds")
plot_ngram_all(dat, "figs/group2-request1.pdf")
plot_group_clown_vomit(data.frame(dat, group = "Group2"), "Group2", width = 30, height = 25)

terms_temp <- read_csv("data/Group1_term_request_09_20_17.csv")
terms <- data.frame(terms = as.character(c(terms_temp$ecologies, terms_temp$ologies)),
  stringsAsFactors = FALSE)
terms <- terms[!is.na(terms$terms), ]
dat <- get_ngram_dat(terms)
saveRDS(dat, file = "data/generated/group1-request1.rds")
dat <- readRDS("data/generated/group1-request1.rds")
plot_ngram_all(dat, "figs/group1-request1.pdf", order_by = "max")
plot_ngram_all(dat, "figs/group1-request1-slope.pdf", order_by = "slope")
plot_ngram_all(dat, "figs/group1-request1-slope-1980-onwards.pdf", order_by = "slope",
  slope_years = 1980:2050)
plot_group_clown_vomit(data.frame(dat, group = "Group1_"), "Group1_", width = 13, height = 10)
# --------------------

terms_temp <- read_table("data/zombie-request1.txt")
dat <- get_ngram_dat(terms_temp$terms)
saveRDS(dat, file = "data/generated/zombie-request1.rds")
dat <- readRDS("data/generated/zombie-request1.rds") %>%
  filter(!gram %in% "rk selection")
plot_ngram_all(dat, "figs/zombie-request1.pdf", order_by = "max",
  year_range = c(1900, 2014), width = 16, height = 12)
plot_group_clown_vomit(data.frame(dat, group = "GroupZ"), "GroupZ", width = 10, height = 8)

# --------------------
# grouped terms

gt <- readr::read_csv("data/Grouped-terms-Sheet1.csv") %>% select(-Notes) %>%
  rename(terms = Term, group = Group) %>%
  transform(terms = tolower(terms), group = tolower(group))
dat_gt <- get_ngram_dat(gt$terms)
saveRDS(dat_gt, file = "data/generated/grouped-terms-dat.rds")
dat_gt <- readRDS("data/generated/grouped-terms-dat.rds")

dat_gt <- left_join(dat_gt, rename(gt, gram = terms))

# -------------
# plot grouped terms

lapply(unique(gt$group), function(x) plot_group_clown_vomit(dat_gt, x))

=======
terms <- read_csv("data/Group2_TermRequest1.csv")
dat1 <- get_ngram_dat(terms$terms)
saveRDS(dat1, file = "data/generated/group2-request1.rds")
dat1 <- readRDS("data/generated/group2-request1.rds")
plot_ngram_all(dat1, "figs/group2-request1.pdf")
plot_group_clown_vomit(data.frame(dat1, group = "Group2"), "Group2",
  width = 30, height = 25, save = TRUE)

terms_temp <- read_csv("data/Group1_term_request_09_20_17.csv")
terms <- data.frame(terms = as.character(c(terms_temp$ecologies, terms_temp$ologies)),
  stringsAsFactors = FALSE)
terms <- terms[!is.na(terms$terms), ]
dat2 <- get_ngram_dat(terms)
saveRDS(dat2, file = "data/generated/group1-request1.rds")
dat2 <- readRDS("data/generated/group1-request1.rds")
plot_ngram_all(dat2, "figs/group1-request1.pdf", order_by = "max")
plot_ngram_all(dat2, "figs/group1-request1-slope.pdf", order_by = "slope")
plot_ngram_all(dat2, "figs/group1-request1-slope-1980-onwards.pdf", order_by = "slope",
  slope_years = 1980:2050)
plot_group_clown_vomit(data.frame(dat, group = "Group1_"), "Group1_",
  width = 13, height = 10, save = TRUE)
# --------------------

terms_temp <- read_table("data/zombie-request1.txt")
dat3 <- get_ngram_dat(terms_temp$terms)
saveRDS(dat3, file = "data/generated/zombie-request1.rds")
dat3 <- readRDS("data/generated/zombie-request1.rds") %>%
  filter(!gram %in% "rk selection")
plot_ngram_all(dat3, "figs/zombie-request1.pdf", order_by = "max",
  year_range = c(1900, 2014), width = 16, height = 12)
plot_group_clown_vomit(data.frame(dat, group = "GroupZ"), "GroupZ", width = 10,
  height = 8, save = TRUE)

# --------------------
# grouped terms

gt <- readr::read_csv("data/Grouped-terms-Sheet1.csv") %>% select(-Notes) %>%
  rename(terms = Term, group = Group) %>%
  transform(terms = tolower(terms), group = tolower(group))
dat_gt <- get_ngram_dat(gt$terms)
saveRDS(dat_gt, file = "data/generated/grouped-terms-dat.rds")
dat_gt <- readRDS("data/generated/grouped-terms-dat.rds")

# dat_gt <- left_join(dat_gt, rename(gt, gram = terms))

# -------------
# plot grouped terms

lapply(unique(gt$group), function(x) plot_group_clown_vomit(dat_gt, x))

# 2017-12-08
# join together and read in new:

d <- bind_rows(list(dat1, dat2, dat3, dat_gt))
d <- d[!duplicated(d),]
tabs <- c("tools", "fields", "scale", "human-impacts", "conservation",
  "social-economic-ecological", "no-home")
d_themes <- list()
for (i in seq_along(tabs)) {
  d_themes[[i]] <- readxl::read_xlsx("data/themes-2017-12-08.xlsx",
    sheet = tabs[[i]])
  d_themes[[i]]$theme <- tabs[[i]]
  d_themes[[i]]$mainpaper <- NULL
  d_themes[[i]] <- filter(d_themes[[i]], !is.na(term))
}
d_themes <- bind_rows(d_themes)
d_themes <- d_themes %>% mutate(term = tolower(term),
  subpanel = tolower(subpanel))

d_themes <- d_themes %>%
  mutate(already_extracted = ifelse(term %in% d$gram, TRUE, FALSE))

d_themes_grams <- filter(d_themes, !already_extracted)$term %>%
  get_ngram_dat()
saveRDS(d_themes_grams, file = "data/generated/d_themes_grams.rds")

aleady_exist <- d[d$gram %in% filter(d_themes, already_extracted)$term, ]
all <- bind_rows(aleady_exist, d_themes_grams)
d_grams <- left_join(all, rename(d_themes, gram = term), by = "gram") %>%
  arrange(theme, subpanel, gram, year) %>%
  select(-already_extracted)

saveRDS(d_grams, file = "data/generated/d_grams.rds")

# -----------
source("analysis/explore_grams.R")

filter(d_grams, subpanel == "conservation") %>%
  explore_grams()

filter(d_grams, subpanel == "conservation") %>%
  mutate(highlight = gram %in% c(
    "conservation",
    "natural history"
    )) %>%
  explore_grams(colour = "highlight")

filter(d_grams, subpanel == "conservation") %>%
  mutate(highlight = gram %in% c(
    "allee effect",
    "diversity"
  )) %>%
  explore_grams(colour = "highlight",
    colour_scale =
      scale_colour_manual(values = c("TRUE" = "red", "FALSE" = "grey60")))

filter(d_grams, subpanel == "conservation") %>%
  mutate(highlight = case_when(
    gram %in% c(
      "conservation",
      "natural history") ~ "a",
    gram %in% c("diversity") ~ "b",
    TRUE ~ "c")) %>%
  explore_grams(colour = "highlight",
    colour_scale = scale_colour_manual(values =
        c(
          "a" = "red",
          "b" = "blue",
          "c" = "grey70")))

test <- get_ngram_dat(c("ecology", "because", "then", "water", "large",
  "animals", "plants", "experiments"))
ecology <- filter(test, gram == "ecology")

test2 <- ngrams1 %>% filter(year %in% 1935) %>%
  group_by(gram) %>%
  summarise(total = sum(count)) %>%
  arrange(-total) %>%
  collect(n = 600)
test2_ <- left_join(test2, filter(total1, year == 1935))


words <- c("water", "large",
  "animals", "plants", "experiments")
test3 <- get_ngram_dat(words)
test3 %>% filter(year <= 2011, year > 1930) %>% ggplot(aes(year, total/total_words*10000, colour = gram)) + geom_line()

test <- get_ngram_dat(c("ecology", "because", "then"))

