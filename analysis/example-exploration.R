library(tidyverse)
d_grams <- readRDS("data/generated/d_grams.rds")
ecology <- readRDS("data/generated/ecology.rds")
d_grams <- inner_join(d_grams, ecology, by = "year")

brent <- readr::read_csv("data/brent.csv", trim_ws = TRUE) %>%
  select(term, panel) %>%
  mutate(term = tolower(term)) %>%
  rename(gram = term) %>%
  inner_join(d_grams, by = "gram") %>%
  filter(year > 1935, year <= 2011)

d <- filter(brent, panel == "1")
d <- d[!duplicated(select(d, year, gram, total)), ]
g <- d %>%
  ggplot(aes(year, total/ecology, group = gram, colour = gram)) +
  guides(colour = FALSE) +
  geom_line(alpha = 0.5) +
  stat_smooth(method = "gam", formula = y ~ s(x), se = FALSE,
    method.args = list(family = gaussian(link = "identity")), lwd = 1) +
  theme_light() +
  ggrepel::geom_text_repel(data = filter(d, year == 2011),
    aes_string(label = "gram"),
    size = 4,
    nudge_x = 10,
    segment.size = 0.2,
    segment.color = "#00000030"
  ) +
  scale_x_continuous(breaks = seq(1935, 2012, 10), limits = c(1935, 2018))
print(g)
