make_handpicked_panel <- function(terms_file, cache_file, fig_file,
                                  fig_width = 6, fig_height = 10,
                                  right_gap = 64, ncols = 2,
                                  overwrite_cache = FALSE, ...) {

  d <- read.csv(terms_file, strip.white = TRUE, stringsAsFactors = FALSE)
  d$gram <- tolower(d$gram)
  d$gram <- gsub("-", " ", d$gram)
  terms <- unique(d$gram)

  N <- unlist(lapply(strsplit(terms, " "), length))
  if (max(N) > 3) {
    warning(
      "More than 3 words: ", paste(terms[N > 3], collapse = ", "), ". ",
      "Removing them."
    )
  }

  if (!file.exists(cache_file) || overwrite_cache) {
    out1 <- gram_db1 %>%
      filter(gram %in% terms[N == 1L]) %>%
      collect(n = Inf) %>%
      inner_join(total1)

    out2 <- gram_db2 %>%
      filter(gram %in% terms[N == 2L]) %>%
      collect(n = Inf) %>%
      inner_join(total1)

    out3 <- get_ngram_dat(terms[N == 3L])

    out <- bind_rows(out1, out2) %>%
      bind_rows(out3) %>%
      unique()

    saveRDS(out, file = cache_file)
  } else {
    out <- readRDS(cache_file)
  }

  d <- full_join(d, out, by = "gram") %>%
    dplyr::filter(!is.na(total)) %>%
    unique()

  if (!"show" %in% names(d)) {
    d <- d %>% mutate(show = "yes")
  }

  pdf(fig_file, width = fig_width, height = fig_height)
  ecogram_panels(d, right_gap = right_gap, ncols = ncols, ...)
  dev.off()

  invisible(d)
}

