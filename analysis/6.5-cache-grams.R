library(tidyverse)
source("analysis/extract-functions.R")

ng1 <- ngrams1 %>%
  filter(year >= 1920, year <= 2011) %>%
  group_by(gram, year) %>%
  summarise(total = sum(count)) %>%
  collect(n = Inf)
saveRDS(ng1, file = "data/generated/condensed-ngrams1.rds")
rm(ng1)

ng2 <- ngrams2 %>%
  filter(year >= 1920, year <= 2011) %>%
  group_by(gram, year) %>%
  summarise(total = sum(count)) %>%
  collect(n = Inf) %>%
  filter(!grepl("[0-9]+", gram)) # no numbers
saveRDS(ng2, file = "data/generated/condensed-ngrams2.rds")
rm(ng2)

d2 <- readRDS("data/generated/condensed-ngrams2.rds")
db2 <- src_sqlite("data/generated/jstor2-condensed.sqlite3", create = TRUE)
copy_to(db2, d2, "ngrams", temporary = FALSE)
rm(d2)

d1 <- readRDS("data/generated/condensed-ngrams1.rds")
db1 <- src_sqlite("data/generated/jstor1-condensed.sqlite3", create = TRUE)
copy_to(db1, d1, "ngrams", temporary = FALSE)
rm(d1)
