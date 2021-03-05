shape_top_decade <- function(data, gram_db, total1, top = 9) {

  keep <- group_by(data, decade, lemma) %>%
    # top_n(n = 1, wt = total) %>%
    summarise(total = sum(total)) %>%
    group_by(decade) %>%
    top_n(n = top, wt = total) %>%
    ungroup() %>%
    select(decade, lemma)

  pop3_keep <- inner_join(data, keep, by = c("decade", "lemma"))

  # extract:
  dat3 <- gram_db %>% filter(gram %in% !!pop3_keep$gram) %>%
    collect(n = Inf) %>%
    left_join(total1, by = "year") %>% # from extract-functions.R
    ungroup()

  # add back lemma column:
  dat4 <- inner_join(dat3, select(pop3_keep, lemma, gram), by = "gram")

  # condense across lemmas:
  condensed_across_lemmas <- dat4 %>%
    group_by(year, total_words, lemma) %>%
    summarize(total = sum(total),
      grams = paste(sort(unique(gram)), collapse = ", "))

  # add decade back:
  dat4 <- full_join(condensed_across_lemmas,
    unique(select(pop3_keep, lemma, decade)), by = "lemma")

  add_zeros <- function(data) {
    plyr::ddply(data, c("decade", "lemma"), function(x) {
      x <- left_join(filter(total1, year >= min(x$year), year <= max(x$year)), x,
        by = c("year", "total_words"))
      missing <- is.na(x$total)
      x$lemma[missing] <- x$lemma[!missing][1]
      x$grams[missing] <- x$grams[!missing][1]
      x$decade[missing] <- x$decade[!missing][1]
      x$total[missing] <- 0
      x
    })
  }
  dat4 <- add_zeros(dat4)

  dat5 <- dat4 %>% mutate(lemma = gsub("specie", "species", lemma))
  dat5 <- dat5 %>% mutate(lemma = gsub("datum", "data", lemma))
  dat5 <- dat5 %>% mutate(lemma = gsub("speciess", "species", lemma))
  dat5 <- group_by(dat5, lemma) %>%
    mutate(list_type = paste(sort(unique(decade)), collapse = " / ")) %>%
    ungroup() %>%
    mutate(list_type2 = grepl(pattern = " / ", list_type)) %>%
    mutate(decade = paste0(decade, "s"))
}
