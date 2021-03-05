library(tidyverse)
decisions <- readr::read_csv("data/journal-filtering.csv")

identifiers <- readr::read_csv("data/terms.csv")

include <- filter(decisions, group_vote == "IN")

include$journal[include$journal == "Ã‰coscience"] <- "Écoscience"

include <- include %>% arrange(journal)

urls <- read_csv("data/generated/url-data.csv")
urls <- inner_join(select(urls, journal, slug), select(include, journal))
urls <- urls[!duplicated(urls),]

stopifnot(nrow(urls) == nrow(include))

write_csv(urls, "data/generated/jstor-journals.csv")

# The format
# http://www.jstor.org/action/doBasicSearch?Query=jo:%22Journal+of+Animal+Ecology%22+OR+jo:%22Science%22

# If too many search terms search will not complete:
i <- c(seq(1, nrow(include), 1), nrow(include))
i <- unique(i)

start <- "http://www.jstor.org/action/doBasicSearch?Query=jo:%22"
end <- "%22"

u <- sapply(seq(1, length(i), 2), function(x) {
  main <- paste0(include$journal[i[x]:i[x+1]], collapse = "%22+OR+jo:%22")
  url <- paste0(start, main, end)
  url
})

writeLines(u, "data/generated/urls.txt")
writeLines(include$journal, "data/generated/included-journals.txt")
