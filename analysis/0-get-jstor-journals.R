library(rvest)
library(tidyverse)

un <- c("ecology", "biologicalsciences")
u <- lapply(un, function(x) read_html(paste0("http://www.jstor.org/subject/", x)))

xp <- "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'mlxl', ' ' ))]"

i <<- 1
y <- lapply(u, function(x) {
  q <- x %>%
    html_nodes("table") %>%
    .[[1]] %>%
    html_table()
  names(q) <- c("access", "journal", "date_range")
  q$subject <- un[[i]]
  i <<- i + 1

  du <- html_nodes(x, xpath = xp) %>%
    html_text() %>%
    gsub("\\n", "", .) %>%
    gsub("    ", "", .)

  url <- html_nodes(x, xpath = "//td") %>%
    html_nodes("a") %>% html_attr("href")
  url <- url[grepl("journal", url)]
  url <- paste0("http://www.jstor.org", url)

  q$url <- url
  q <- mutate(q, continuation = journal %in% du)

  q
})

d <- bind_rows(y)

d2 <- mutate(d, start = sub("([0-9]+) - [0-9]*", "\\1", date_range),
  end = sub("([0-9]+) - ([0-9]*)", "\\2", date_range)) %>%
  select(-date_range, -access)

d3 <- group_by(d2, subject) %>%
  mutate(continuation_id = paste(cumsum(!continuation), subject, sep = "-")) %>%
  group_by(continuation_id) %>%
  mutate(continuation = ifelse(n() > 1, 1, 0)) %>%
  ungroup() %>%
  group_by(journal) %>%
  summarise(subject = paste(sort(unique(subject)), collapse = "; "),
    continuation = max(continuation),
    continuation_id = paste(sort(unique(continuation_id)), collapse = "; "),
    start = min(start),
    end = max(end),
    duration = as.numeric(end) - as.numeric(start),
    continuation_id = ifelse(max(continuation) == 1, continuation_id, ""),
    url = url[1]) %>%
  ungroup() %>%
  arrange(continuation_id, start) %>%
  select(-continuation)

readr::write_csv(d3, "data/generated/jstor-journals.csv")
