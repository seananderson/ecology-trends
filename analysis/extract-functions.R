library(tidyverse)
library(ggsidekick)
library(forcats)
library(directlabels)

pnas_exluded_pub_ids <- readRDS("data/generated/pnas_exluded_pub_ids.rds")

filtered_journals <-
  readr::read_csv(
    "data/taxa-specific-journal-classifications.csv",
    col_types = cols(
      Journal = col_character(),
      Slug = col_character(),
      TaxonSpecific = col_character(),
      Taxon = col_character(),
      Notes = col_character()
    )
  ) %>%
  filter(TaxonSpecific == "N") %>%
  select(Slug) %>% rename(journal = Slug)

bad_amnat <- readr::read_lines("data/bad-amnat.txt") # latex code

d1 <- dplyr::src_sqlite("data/jstor1.sqlite3")
ngrams1 <- dplyr::tbl(d1, "ngrams") %>% filter(year != 999) %>%
  filter(year >= 1920, year <= 2014) %>%
  filter(!(gram %in% bad_amnat & journal == "amernatu")) %>%
  filter(!pub_id %in% pnas_exluded_pub_ids,
    journal %in% filtered_journals$journal)

d2 <- dplyr::src_sqlite("data/jstor2.sqlite3")
ngrams2 <- dplyr::tbl(d2, "ngrams") %>% filter(year != 999) %>%
  filter(year >= 1920, year <= 2014) %>%
  filter(!pub_id %in% pnas_exluded_pub_ids,
    journal %in% filtered_journals$journal)

d3 <- dplyr::src_sqlite("data/jstor3.sqlite3")
ngrams3 <- dplyr::tbl(d3, "ngrams") %>% filter(year != 999) %>%
  filter(year >= 1920, year <= 2014) %>%
  filter(!pub_id %in% pnas_exluded_pub_ids,
    journal %in% filtered_journals$journal)

if (!file.exists("data/generated/total1.rds")) {
  total1 <- ngrams1 %>%
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

  dd <- filter(terms, n == 1)
  if (nrow(dd) > 0) {
    message("Extracting 1 grams")
    ecology1 <- ngrams1 %>% filter(gram %in% dd$terms) %>%
      group_by(year, gram) %>%
      summarise(total = sum(count)) %>%
      collect(n = Inf) %>%
      left_join(total1, by = "year")
  }

  dd <- filter(terms, n == 2)
  if (nrow(dd) > 0) {
    message("Extracting 2 grams")
    ecology2 <- ngrams2 %>% filter(gram %in% dd$terms) %>%
      group_by(year, gram) %>%
      summarise(total = sum(count)) %>%
      collect(n = Inf) %>%
      left_join(total1, by = "year")
  }

  dd <- filter(terms, n == 3)
  if (nrow(dd) > 0) {
    message("Extracting 3 grams")
    ecology3 <- ngrams3 %>% filter(gram %in% dd$terms) %>%
      group_by(year, gram) %>%
      summarise(total = sum(count)) %>%
      collect(n = Inf) %>%
      left_join(total1, by = "year")
  }
  dat <- bind_rows(list(ecology1, ecology2, ecology3)) %>%
    ungroup()
  filter(dat, !is.na(gram))
}

plot_ngram_all <- function(data, filename = "figs/x.pdf", width = 33,
  height = 22, order_by = "max", slope_years = c(1935:2011),
  scales = "free_y", log_y = FALSE, year_range = c(1935, 2011)) {

  data <- filter(data, year <= year_range[2], year >= year_range[1])

  if (order_by == "max") {
    data <- data %>%
      ungroup() %>% group_by(gram) %>%
      mutate(order_column = max(total/total_words))
  }
  if (order_by == "slope") {
    data_sum <- data %>% filter(year %in% slope_years) %>%
      ungroup() %>% group_by(gram) %>%
      summarise(order_column =
          coef(lm(log(.data$total/.data$total_words) ~ .data$year))[[2]])
    data <- left_join(data, data_sum, by = "gram")
  }

  g <- data %>%
    ggplot(aes(year, (total/ecology)*1e6)) +
    geom_point(alpha = 0.7, colour = "grey40") +
    geom_line(alpha = 0.5) +
    geom_smooth(colour = "red", method = "gam",
      method.args = list(family = gaussian(link = "identity")),
      formula = y ~ s(x), se = FALSE) +
    theme_sleek() +
    facet_wrap(~fct_reorder(gram, -order_column), scales = scales) +
    ylab("Instances per million words")

  if (log_y) g <- g + scale_y_log10()

  ggsave(filename, width = width, height = height)
}

plot_group_clown_vomit <- function(dat, group_name, width = 30, height = 25,
  save = FALSE) {
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
      error = function(ignore)
        gam(I(log10(total/total_words*10000)) ~ s(year, k = 3), data = x)
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

  if (save)
    ggsave(paste0("figs/grouped-", group_name, ".pdf"), width = width,
      height = height)
}
