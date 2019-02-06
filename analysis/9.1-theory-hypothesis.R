source("analysis/extract-functions.R")
source("analysis/pretty-panels.R")

gram_db2 <- dplyr::src_sqlite("data/generated/jstor2-condensed.sqlite3") %>%
  dplyr::tbl("ngrams")

blank_grams <- gram_db2 %>%
  filter(year %in% c(1930:2010)) %>%
  filter(
    gram %like% "% community" |
      gram %like% "% ecosystem" |
      gram %like% "% niche" |
      gram %like% "% species" |

      gram %like% "ecosystem %" |
      gram %like% "community %" |
      gram %like% "species %" |
      gram %like% "population %" |

      gram %like% "niche %" |
      gram %like% "% niche" |

      gram %like% "% theory" |
      gram %like% "% hypothesis" |

      gram %like% "% experiment" |
      gram %like% "% experiments" |
      # gram %like% "% data" |
      # gram %like% "% model" |
      # gram %like% "% models" |
      gram %like% "% analysis" |
      # gram %like% "% variation" |
      gram %like% "% variability" |
      # gram %like% "% scale" |
      gram %like% "% ecosystem" |
      gram %like% "% ecosystems" |
      gram %like% "% diversity" |
      gram %like% "% population" |
      gram %like% "% populations" |
      gram %like% "% distribution" |
      gram %like% "% conservation"
  ) %>%
  # group_by(year, gram) %>%
  # summarise(total = sum(count)) %>%
  collect(n = Inf)

save(blank_grams, file = "data/generated/blank_grams.rda")
load("data/generated/blank_grams.rda")

blank_grams3 <- ngrams3 %>%
  filter(year %in% c(1930:2010)) %>%
  filter(
    gram %like% "theory of %" |
      gram %like% "hypothesis of %" |
      gram %like% "% theory" |
      gram %like% "% hypothesis"
  ) %>%
  group_by(year, gram) %>%
  summarise(total = sum(count)) %>%
  collect(n = Inf)

save(blank_grams3, file = "data/generated/blank_grams3.rda")
load("data/generated/blank_grams3.rda")

get_n_term <- function(x, n) {
  unlist(lapply(x, function(.x) strsplit(.x, " ")[[1]][n]))
}

blank_grams3 <- blank_grams3 %>%
  mutate(gram1 = get_n_term(gram, 1),
    gram2 = get_n_term(gram, 2),
    gram3 = get_n_term(gram, 3))

blank_grams3_swap <- filter(blank_grams3,
  gram1 %in% c("theory", "hypothesis"))
blank_grams3_noswap <- filter(blank_grams3,
  !gram1 %in% c("theory", "hypothesis"))

blank_grams3_swap <- mutate(blank_grams3_swap,
  temp_word = gram1,
  gram1 = gram2,
  gram2 = gram3,
  gram3 = paste(temp_word, "as first")) %>%
  select(-temp_word)

blank_grams3 <- bind_rows(blank_grams3_noswap, blank_grams3_swap)

blank_grams <- blank_grams %>%
  mutate(gram1 = get_n_term(gram, 1),
    gram2 = get_n_term(gram, 2))

blank_grams <- filter(blank_grams, gram2 %in% c('theory', 'hypothesis'))

blank_grams <- mutate(blank_grams, main_term = gram2)
blank_grams3 <- mutate(blank_grams3, main_term = get_n_term(gram3, 1))

bg <- bind_rows(blank_grams, blank_grams3)

# ignore <- c('the', 'this', 'of', 'an', 'a', 'by', 'on', 'our', 'on')

bg <- mutate(bg, decade = paste0(substr(year, 1, 3), '0'))
group_by(bg, main_term, decade, gram) %>%
  summarise(sum_total = sum(total)) %>%
  arrange(main_term, decade, -sum_total) %>%
  top_n(50) %>%
  readr::write_csv('data/generated/theory-hypothesis-top-50.csv')

# ------------------------------------------------------------------------------

blank_grams3 %>% group_by(gram) %>% summarise(sum_total = sum(total), gram3 = gram3[[1]]) %>% filter(sum_total > 50) %>% arrange(gram3, -sum_total) %>% rename(panel = gram3) %>% readr::write_csv("data/generated/blank-3-grams-2018-08-16.csv")

blank_grams3 %>% left_join(total1) %>%
  saveRDS("data/generated/blank-3-grams-2018-08-16-raw.rds")
