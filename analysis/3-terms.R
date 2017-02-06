d <- readr::read_csv("data/terms.csv", col_names = FALSE)

f <- function(x) {
  x <- x[,which(!is.na(x))]
  paste(as.character(x[1,]), collapse = " | ")
}

b <- plyr::alply(d, 1, f)

bb <- as.data.frame(plyr::ldply(b)[,"V1"])

readr::write_csv(bb, path = "data/generated/terms.csv")
