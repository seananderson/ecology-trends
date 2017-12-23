library(tidyverse)
library(ggsidekick)
library(forcats)
library(directlabels)

pnas_exluded_pub_ids <- readRDS("data/generated/pnas_exluded_pub_ids.rds")

filtered_journals <- readr::read_csv("data/taxa-specific-journal-classifications.csv") %>%
  filter(TaxonSpecific == "N") %>%
  select(Slug) %>% rename(journal = Slug)

d1 <- dplyr::src_sqlite("data/jstor1.sqlite3")
temp1 <- dplyr::tbl(d1, "ngrams") %>% filter(year != 999) %>%
  filter(!pub_id %in% pnas_exluded_pub_ids, journal %in% filtered_journals$journal)

d2 <- dplyr::src_sqlite("data/jstor2.sqlite3")
temp2 <- dplyr::tbl(d2, "ngrams") %>% filter(year != 999) %>%
  filter(!pub_id %in% pnas_exluded_pub_ids, journal %in% filtered_journals$journal)

d3 <- dplyr::src_sqlite("data/jstor3.sqlite3")
temp3 <- dplyr::tbl(d3, "ngrams") %>% filter(year != 999) %>%
  filter(!pub_id %in% pnas_exluded_pub_ids, journal %in% filtered_journals$journal)

if (!file.exists("data/generated/total1.rds")) {
  total1 <- temp1 %>%
    group_by(year) %>%
    summarise(total_words = sum(count)) %>%
    collect(n = Inf)
  saveRDS(total1, file = "data/generated/total1.rds")
} else {
  total1 <- readRDS("data/generated/total1.rds")
}

get_ngram_dat <- function(terms) {

  terms <- data.frame(terms = as.character(terms), stringsAsFactors = FALSE)
  terms$n <- unlist(lapply(strsplit(terms$terms, " "), length))
  terms$terms <- tolower(terms$terms)
  terms <- mutate(terms,
    terms = sub("^[ ]+", "", terms),
    terms = sub("[ ]+$", "", terms))
  terms <- terms[!duplicated(terms), ]


  if (nrow(filter(terms, n > 3)) > 1) warning("Some > 3 grams")

  ecology1 <- ecology2 <- ecology3 <- data.frame(year = NA, gram = NA,
    total = NA, total_words = NA)

  message("Extracting 1 grams")
  dd <- filter(terms, n == 1)
  if (nrow(dd) > 0) {
    ecology1 <- temp1 %>% filter(gram %in% dd$terms) %>%
      group_by(year, gram) %>%
      summarise(total = sum(count)) %>%
      collect(n = Inf) %>%
      left_join(total1)
  }

  message("Extracting 2 grams")
  dd <- filter(terms, n == 2)
  if (nrow(dd) > 0) {
    ecology2 <- temp2 %>% filter(gram %in% dd$terms) %>%
      group_by(year, gram) %>%
      summarise(total = sum(count)) %>%
      collect(n = Inf) %>%
      left_join(total1)
  }

  message("Extracting 3 grams")
  dd <- filter(terms, n == 3)
  if (nrow(dd) > 0) {
    ecology3 <- temp3 %>% filter(gram %in% dd$terms) %>%
      group_by(year, gram) %>%
      summarise(total = sum(count)) %>%
      collect(n = Inf) %>%
      left_join(total1)
  }
  dat <- bind_rows(list(ecology1, ecology2, ecology3))
  filter(dat, !is.na(gram))
}

plot_ngram_all <- function(data, filename = "figs/x.pdf", width = 33, height = 22,
  order_by = "max", slope_years = c(1500:2050), scales = "free_y",
  log_y = FALSE, year_range = c(1920, 2011)) {

  data <- filter(data, year <= year_range[2], year >= year_range[1])

  if (order_by == "max") {
    data <- data %>%
      ungroup() %>% group_by(gram) %>%
      mutate(order_column = max(total/total_words))
  }
  if (order_by == "slope") {
    data_sum <- data %>% filter(year %in% slope_years) %>%
      ungroup() %>% group_by(gram) %>%
      summarise(order_column = coef(lm(log(.data$total/.data$total_words) ~ .data$year))[[2]])
    data <- left_join(data, data_sum, by = "gram")
  }

  g <- data %>%
    ggplot(aes(year, (total/total_words)*1e6)) +
    geom_point(alpha = 0.7, colour = "grey40") +
    geom_smooth(colour = "red", method = "gam",
      method.args = list(family = Gamma(link = "log")),
      formula = y ~ s(x), se = FALSE) +
    theme_sleek() +
    facet_wrap(~fct_reorder(gram, -order_column), scales = scales) +
    ylab("Instances per million words")

  if (log_y) g <- g + scale_y_log10()

  ggsave(filename, width = width, height = height)
}

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



