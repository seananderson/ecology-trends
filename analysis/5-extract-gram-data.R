library(tidyverse)
library(ggsidekick)
library(forcats)
library(directlabels)

pnas_exluded_pub_ids <- readRDS("data/generated/pnas_exluded_pub_ids.rds")

d1 <- dplyr::src_sqlite("data/jstor1.sqlite3")
temp1 <- dplyr::tbl(d1, "ngrams") %>% filter(year != 999) %>%
  filter(!pub_id %in% pnas_exluded_pub_ids)

d2 <- dplyr::src_sqlite("data/jstor2.sqlite3")
temp2 <- dplyr::tbl(d2, "ngrams") %>% filter(year != 999) %>%
  filter(!pub_id %in% pnas_exluded_pub_ids)

d3 <- dplyr::src_sqlite("data/jstor3.sqlite3")
temp3 <- dplyr::tbl(d3, "ngrams") %>% filter(year != 999) %>%
  filter(!pub_id %in% pnas_exluded_pub_ids)

total1 <- readRDS("data/generated/total1.rds")

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

  dd <- filter(terms, n == 1)
  if (nrow(dd) > 0) {
    ecology1 <- temp1 %>% filter(gram %in% dd$terms) %>%
      group_by(year, gram) %>%
      summarise(total = sum(count)) %>%
      collect(n = Inf) %>%
      left_join(total1)
  }

  dd <- filter(terms, n == 2)
  if (nrow(dd) > 0) {
    ecology2 <- temp2 %>% filter(gram %in% dd$terms) %>%
      group_by(year, gram) %>%
      summarise(total = sum(count)) %>%
      collect(n = Inf) %>%
      left_join(total1)
  }

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

terms <- read_csv("data/Group2_TermRequest1.csv")
dat <- get_ngram_dat(terms$terms)
saveRDS(dat, file = "data/generated/group2-request1.rds")
dat <- readRDS("data/generated/group2-request1.rds")
plot_ngram_all(dat, "figs/group2-request1.pdf")

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

# --------------------

terms_temp <- read_table("data/zombie-request1.txt")
dat <- get_ngram_dat(terms_temp$terms)
saveRDS(dat, file = "data/generated/zombie-request1.rds")
dat <- readRDS("data/generated/zombie-request1.rds") %>%
  filter(!gram %in% "rk selection")
plot_ngram_all(dat, "figs/zombie-request1.pdf", order_by = "max",
  year_range = c(1900, 2014), width = 16, height = 12)
