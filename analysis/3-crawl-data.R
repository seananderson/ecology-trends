library(tidyverse)
library(purrr)
library(assertthat)
rm(list = ls()) # need all memory possible

#' Insert JSTOR data into a database
#'
#' @param Path to the JSTOR files
#' @param db A database connection of class src_sqlite
#' @param ngram 1, 2, or 3 for number of words
#' @param max_n Maximum number of papers to read in at once. This should be as
#'   big as possible without running out of memory.
insert_data <- function(path, db, ngram = 1, max_n = 5000L) {

  assert_that(dir.exists(path))
  assert_that(is.character(path))
  assert_that(is.numeric(ngram))
  assert_that(ngram <= 3)
  assert_that(is.numeric(max_n))
  assert_that(max_n >= 2)
  assert_that(identical(class(db)[[1]], "src_sqlite"))

  x <- dir(path, recursive = TRUE, pattern = "*.txt")
  x <- x[grepl(paste0("NGRAMS", ngram, ".txt"), x)]
  divs <- seq(1, length(x), max_n)
  if (max(divs) < length(x)) divs <- c(divs, length(x))
  for (i in seq(1, length(divs) - 1)) {
    message(paste("Extracting", divs[i], "-", divs[i+1], "of", max(divs)))
    x_temp <- x[divs[i]:divs[i+1]]
    d <- data.frame(journal = purrr::map_chr(x_temp,
      function(a) strsplit(a, "/")[[1]][[1]]), stringsAsFactors = FALSE)
    d$year <- map_chr(x_temp, function(a) strsplit(a, "/")[[1]][[2]])
    d$data <- map(file.path(path, x_temp), function(a) {
      tryCatch(read_tsv(a, progress = FALSE, col_names = c("gram", "count"),
        col_types = cols(gram = col_character(), count = col_integer())),
        error = function(e) data.frame(gram = NA, count = NA))})
    db_insert_into(con = db$con, table = "ngrams", values = unnest(d, data))
    rm(d)
  }
}

# if (file.exists("data/jstor1.sqlite3"))
#   file.remove("data/jstor1.sqlite3")
db1 <- src_sqlite("data/jstor1.sqlite3", create = TRUE)
# copy_to(db1, data.frame(journal = "a", year = 999L, gram = "a", count = 0L,
#   stringsAsFactors = FALSE), "ngrams", temporary = FALSE)
#
# if (file.exists("data/jstor2.sqlite3"))
#   file.remove("data/jstor2.sqlite3")
db2 <- src_sqlite("data/jstor2.sqlite3", create = TRUE)
# copy_to(db2, data.frame(journal = "a", year = 999L, gram = "a", count = 0L,
#   stringsAsFactors = FALSE), "ngrams", temporary = FALSE)
#
# if (file.exists("data/jstor3.sqlite3"))
#   file.remove("data/jstor3.sqlite3")
# db3 <- src_sqlite("data/jstor3.sqlite3", create = TRUE)
# copy_to(db3, data.frame(journal = "a", year = 999L, gram = "a", count = 0L,
#   stringsAsFactors = FALSE), "ngrams", temporary = FALSE)

insert_data("~/Downloads/receipt-id-334441-jcodes-abcde-part-001", db1, ngram = 1)
insert_data("~/Downloads/receipt-id-334441-jcodes-abcde-part-002", db1, ngram = 1)
insert_data("~/Downloads/receipt-id-334441-jcodes-fghijlmnop-part-001", db1, ngram = 1)
insert_data("~/Downloads/receipt-id-334441-jcodes-fghijlmnop-part-002", db1, ngram = 1)
insert_data("~/Downloads/receipt-id-334441-jcodes-fghijlmnop-part-003", db1, ngram = 1)

insert_data("~/Downloads/receipt-id-334441-jcodes-abcde-part-001", db2, ngram = 2)
insert_data("~/Downloads/receipt-id-334441-jcodes-abcde-part-002", db2, ngram = 2)
insert_data("~/Downloads/receipt-id-334441-jcodes-fghijlmnop-part-001", db2, ngram = 2)
insert_data("~/Downloads/receipt-id-334441-jcodes-fghijlmnop-part-002", db2, ngram = 2)
insert_data("~/Downloads/receipt-id-334441-jcodes-fghijlmnop-part-003", db2, ngram = 2)
