library(tidyverse)
source("analysis/extract-functions.R")

ng1 <- ngrams1 %>%
  filter(year >= 1920, year <= 2011) %>%
  group_by(gram, year) %>%
  summarise(total = sum(count)) %>%
  collect(n = Inf)
saveRDS(ng1, file = "data/generated/condensed-ngrams1.rds")

ng2 <- ngrams2 %>%
  filter(year >= 1920, year <= 2011) %>%
  group_by(gram, year) %>%
  summarise(total = sum(count)) %>%
  collect(n = Inf) %>%
  filter(!grepl("[0-9]+", gram)) # no numbers
saveRDS(ng2, file = "data/generated/condensed-ngrams2.rds")

