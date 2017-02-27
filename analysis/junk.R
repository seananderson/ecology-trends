library(readr)
library(dplyr)
library(tidyr)
library(purrr)

p <- "~/Downloads/jstor/receipt-id-120061-jcodes-abcde-part-004"
x <- dir(p, recursive = TRUE, pattern = "*.txt")

# x <- file.path(p, x[1:2000])
# x <- gsub("~", "/Users/seananderson", x)
if (file.exists("data/jstor.sqlite3"))
  system("rm data/jstor.sqlite3")
# zz <- file("sql.txt", "w")
# cat("CREATE TABLE ngrams(\n", file = zz)
# cat("gram CHAR(255),\n", file = zz)
# cat("count INT);\n", file = zz)
# cat(".mode tabs", file = zz)
# cat("\n", file = zz)
# out <- lapply(x, function(a) cat(paste0(".import ", a, " ngrams", "\n"), file = zz))
# close(zz)
# system("sqlite3 data/jstor.sqlite3 < sql.txt")
#
# # sqlite3 mydatabase
#
# CREATE TABLE ngrams(
#   journal CHAR(25) NOT NULL,
#   year INT,
#   gram CHAR(255),
#   count INT
# );
#
# CREATE TABLE ngrams(
#   gram CHAR(255),
#   count INT
# );
#
# CREATE TABLE IF NOT EXISTS ngrams(
#   gram CHAR(255),
#   count integer
# );
#
# .mode tabs
# .import /Users/seananderson/Downloads/jstor/receipt-id-120061-jcodes-abcde-part-004/abstbota/1976/4/i40139788/43519364/43519364-NGRAMS4.txt ngrams
#
# .import ~/Desktop/test.txt ngrams
