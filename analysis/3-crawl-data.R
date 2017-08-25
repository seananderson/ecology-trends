library(tidyverse)
library(purrr)
library(assertthat)
rm(list = ls()) # need all memory possible

#' Insert JSTOR data into a database
#'
#' @param path to the JSTOR files
#' @param db A database connection of class src_sqlite
#' @param ngram 1, 2, or 3 for number of words
#' @param max_n Maximum number of papers to read in at once. This should be as
#'   big as possible without running out of memory.
insert_data <- function(path, db, ngram = 1, max_n = 6000L,
  journal_filter = NULL, save_pub_id = TRUE) {

  assert_that(dir.exists(path))
  assert_that(is.character(path))
  assert_that(is.numeric(ngram))
  assert_that(ngram <= 3)
  assert_that(is.numeric(max_n))
  assert_that(max_n >= 2)
  assert_that(identical(class(db)[[1]], "src_dbi"))

  x <- dir(path, recursive = TRUE, pattern = "*.txt")
  x <- x[grepl(paste0("NGRAMS", ngram, ".txt"), x)]

  if (!is.null(journal_filter)) {
    x <- x[grepl(paste0("^", journal_filter, "\\/"), x)]
    if (length(x) == 0) return(NA)
  }
  divs <- seq(0, length(x), max_n)

  if (max(divs) < length(x)) divs <- c(divs, length(x))
  for (i in seq(1, length(divs) - 1)) {
    message(paste("Extracting", (divs[i] + 1), "-", divs[i+1], "of", max(divs)))
    x_temp <- x[(divs[i] + 1):divs[i + 1]]
    d <- data.frame(
      journal = purrr::map_chr(x_temp, function(a) strsplit(a, "/")[[1]][[1]]),
      stringsAsFactors = FALSE)

    if (save_pub_id) d$pub_id <-
      purrr::map_chr(x_temp, function(a) {
        sp <- strsplit(a, "/")[[1]]
        sp <- sp[[length(sp)]]
        as.numeric(gsub("-NGRAM[S0-9]+\\.txt", "", sp))})

    d$year <- map_chr(x_temp, function(a) strsplit(a, "/")[[1]][[2]])
    d$data <- map(file.path(path, x_temp), function(a) {
      tryCatch(read_tsv(a, progress = FALSE, col_names = c("gram", "count"),
        col_types = cols(gram = col_character(), count = col_integer())),
        error = function(e) data.frame(gram = NA, count = NA))})
    db_insert_into(con = db$con, table = "ngrams", values = unnest(d, data))
    rm(d)
  }
}

db1 <- src_sqlite("data/jstor1.sqlite3", create = TRUE)
copy_to(db1, data.frame(journal = "a", pub_id = 999L, year = 999L, gram = "a", count = 0L,
  stringsAsFactors = FALSE), "ngrams", temporary = FALSE)

db2 <- src_sqlite("data/jstor2.sqlite3", create = TRUE)
copy_to(db2, data.frame(journal = "a", pub_id = 999L, year = 999L, gram = "a", count = 0L,
  stringsAsFactors = FALSE), "ngrams", temporary = FALSE)

db3 <- src_sqlite("data/jstor3.sqlite3", create = TRUE)
copy_to(db3, data.frame(journal = "a", pub_id = 999L, year = 999L, gram = "a", count = 0L,
  stringsAsFactors = FALSE), "ngrams", temporary = FALSE)

f <- list.files("data/raw", full.names = TRUE)

system.time({
  lapply(f, function(x) insert_data(x, db1, ngram = 1))
})
system.time({
  lapply(f, function(x) insert_data(x, db2, ngram = 2))
})
system.time({
  lapply(f, function(x) insert_data(x, db3, ngram = 3))
})
