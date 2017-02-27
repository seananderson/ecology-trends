library(readr)
library(dplyr)
library(tidyr)
library(purrr)

p <- "~/Downloads/jstor/receipt-id-120061-jcodes-abcde-part-004"
x <- dir(p, recursive = TRUE, pattern = "*.txt")

if (file.exists("data/jstor.sqlite3"))
  system("rm data/jstor.sqlite3")

max_n <- 500
divs <- seq(1, length(x), max_n)
if (max(divs) < length(x)) divs <- c(divs, length(x))

db <- dplyr::src_sqlite("data/jstor.sqlite3", create = TRUE)
dplyr::copy_to(db, data.frame(journal = "a", year = 999L, gram = "a", count = 0L,
  stringsAsFactors = FALSE),
  "ngrams", temporary = FALSE)

for (i in seq(1, length(divs) - 1)) {
  message(paste("Extracting", divs[i], "-", divs[i+1], "of", max(divs)))
  x_temp <- x[divs[i]:divs[i+1]]
  d <- data.frame(journal = purrr::map_chr(x_temp, function(a) strsplit(a, "/")[[1]][[1]]),
    stringsAsFactors = FALSE)
  d$year <- map_chr(x_temp, function(a) strsplit(a, "/")[[1]][[2]])
  # d$data <- map(file.path(p, x_temp), function(a) {
    # tryCatch(data.table::fread(a, sep = "\t", data.table = FALSE),
      # error = function(e) data.frame(V1 = NA, V2 = NA))})
  d$data <- map(file.path(p, x_temp), function(a) {
    tryCatch(read_tsv(a, progress = FALSE, col_names = c("gram", "count"),
      col_types = cols(gram = col_character(), count = col_integer())),
      error = function(e) data.frame(gram = NA, count = NA))})

  dat <- unnest(d, data)
  # dat <- rename(dat, gram = V1, count = V2)

  db_insert_into(con = db$con, table = "ngrams", values = dat)

  rm(dat)
  rm(d)
}

# dd <- dplyr::src_sqlite("data/jstor.sqlite3")
# temp <- dplyr::tbl(dd, "ngrams")
# dplyr::glimpse(temp)
