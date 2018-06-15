make_handpicked_panel <- function(terms_file, cache_file, fig_file,
                                  fig_width = 6, fig_height = 10,
                                  right_gap = 64, ncols = 2,
                                  overwrite_cache = FALSE, ...) {

  d <- read.csv(terms_file, strip.white = TRUE, stringsAsFactors = FALSE)

  d <- d %>% dplyr::group_by(panel) %>%
    dplyr::mutate(order = unique(order[!is.na(order)])) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(panel = paste(order, panel, sep = ": ")) %>%
    dplyr::select(-order)

  d$gram <- tolower(d$gram)
  d$gram <- gsub("-", " ", d$gram)
  terms  <- unique(d$gram)

  N <- unlist(lapply(strsplit(terms, " "), length))
  if (max(N) > 3) {
    warning("More than 3 words: ", 
      paste(terms[N > 3], collapse = ", "), ". ",
      "Removing them.", call. = FALSE)
  }

  if (!file.exists(cache_file) || overwrite_cache) {
    message("Extracting 1 grams")
    out1 <- gram_db1 %>%
      dplyr::filter(gram %in% terms[N == 1L]) %>%
      dplyr::collect(n = Inf) %>%
      dplyr::inner_join(total1, by = "year")

    message("Extracting 2 grams")
    out2 <- gram_db2 %>%
      dplyr::filter(gram %in% terms[N == 2L]) %>%
      dplyr::collect(n = Inf) %>%
      dplyr::inner_join(total1, by = "year")

    out3 <- get_ngram_dat(terms[N == 3L])

    out <- dplyr::bind_rows(out1, out2) %>%
      dplyr::bind_rows(out3) %>%
      unique()

    readr::write_rds(out, cache_file)
  } else {
    out <- readr::read_rds(cache_file)
  }

  d <- dplyr::full_join(d, out, by = "gram") %>%
    dplyr::filter(!is.na(total)) %>%
    unique()

  grDevices::pdf(fig_file, width = fig_width, height = fig_height)
  ecogram_panels(d, right_gap = right_gap, ncols = ncols, ...)
  grDevices::dev.off()

  invisible(d)
}
