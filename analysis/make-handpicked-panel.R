source("analysis/frontiers_theme.R")
make_handpicked_panel <- function(terms_file, cache_file, fig_file,
  fig_width = 6, fig_height = 10, bottom_frac_up = 0.035,
  right_gap = 64, ncols = 2, connector_length = 1.9,
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
    .terms <- terms[N == 1L]
    out1 <- gram_db1 %>%
      dplyr::filter(gram %in% .terms) %>%
      dplyr::collect(n = Inf) %>%
      dplyr::inner_join(total1, by = "year")

    message("Extracting 2 grams")
    .terms <- terms[N == 2L]
    out2 <- gram_db2 %>%
      dplyr::filter(gram %in% .terms) %>%
      dplyr::collect(n = Inf) %>%
      dplyr::inner_join(total1, by = "year")

    if (max(N) == 3) {
      .terms <- terms[N == 3L]
      out3 <- get_ngram_dat(.terms)
      out <- dplyr::bind_rows(out1, out2) %>%
        dplyr::bind_rows(out3) %>%
        unique()
    } else {
      out <- dplyr::bind_rows(out1, out2) %>%
        unique()
    }

    readr::write_rds(out, cache_file)
  } else {
    out <- readr::read_rds(cache_file)
  }

  d <- dplyr::left_join(d, out, by = "gram") %>%
    dplyr::filter(!is.na(total)) %>%
    unique()

  d$gram_canonical <- gsub("single nucleotide polymorphism",
    "single nucleotide\\\npolymorphism\\\n", d$gram_canonical)
  d$gram_canonical <- gsub("supplementary material",
    "\\\nsupplementary\\\nmaterial", d$gram_canonical)
  d$gram_canonical <- gsub("supplementary material",
    "\\\nsupplementary\\\nmaterial", d$gram_canonical)
  d$gram_canonical <- gsub("personal communication",
    "personal\\\ncommunication", d$gram_canonical)
  d$gram_canonical <- gsub(" sequencing",
    " seq.", d$gram_canonical)
  d$gram_canonical <- gsub("marine protected area",
    "marine protected\\\narea", d$gram_canonical)
  d$gram_canonical <- gsub("harvesting/collecting",
    "\\\n\\\nharvesting/\\\ncollecting", d$gram_canonical)
  d$gram_canonical <- gsub("theory of island \\[biogeography\\]",
    "theory of island ...", d$gram_canonical)
  d$gram_canonical <- gsub("theory of natural \\[selection\\]",
    "theory of natural ...", d$gram_canonical)
  d$gram_canonical <- gsub("community-based management",
    "\\\ncommunity-based\\\nmanagement", d$gram_canonical)
  d$gram_canonical <- gsub("human-wildlife conflict",
    "human-wildlife\\\nconflict", d$gram_canonical)
  d$gram_canonical <- gsub("participant observation",
    "participant\\\nobservation", d$gram_canonical)
  d$gram_canonical <- gsub("focus group",
    "focus group\\\n", d$gram_canonical)

  d$panel <- gsub("Consequences to species/landscapes",
    "Landscape/spp. consequences", d$panel)

  if (any(grepl("[0-9]+: Hypotheses$", d$panel))) {
    d$gram_canonical <- gsub(" hypothesis$",
      " ...", d$gram_canonical)
  }

  pdf(fig_file, width = fig_width, height = fig_height)
  par(lheight = 0.65)
  dat <- ecogram_panels(d, right_gap = right_gap, ncols = ncols,
    bottom_frac_up = bottom_frac_up, connector_length = connector_length,
    ...)
  dev.off()

  dat$gram_canonical <- gsub("([a-zA-Z]+)\\\n", "\\1 ", dat$gram_canonical)
  dat$gram_canonical <- gsub("^\\\n\\\n", "", dat$gram_canonical)
  dat$gram_canonical <- gsub("^\\\n", "", dat$gram_canonical)
  dat$gram_canonical <- gsub("\\\n$", "", dat$gram_canonical)
  dat$gram_canonical <- gsub("/\\\n", "/", dat$gram_canonical)
  # print(unique(dat$gram_canonical))

  g <- ggplot(dat, aes(year, total / total_words * 100000)) +
    geom_point(size = 1, pch = 21) +
    geom_line() +
    facet_wrap(~paste(gram_canonical, gram, sep = "\n"),
      scales = "free_y", ncol = 6) +
    frontiers_theme() +
    ylab("Frequency per 100,000 words") +
    xlab("Year")
  invisible(g)
}
