library(tidyverse)
decisions <- readr::read_csv("data/journal-filtering.csv")

# if (interactive()) View(d)
# if (interactive()) View(journals)

include <- filter(decisions, group_vote == "IN")

include$journal[include$journal == "Ã‰coscience"] <- "Écoscience"

include <- include %>% arrange(journal)

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
