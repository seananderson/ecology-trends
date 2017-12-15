library(tidyverse)
grams <- readRDS("data/generated/becky-2017-12-14.rds")
d <- readr::read_csv("data/scale_panels_1_nohyphens.csv")
d <- left_join(d, grams)

# the first time, run:
# install.packages("devtools")
# devtools::install_github("seananderson/ggsidekick")

temp <- d %>%
  group_by(gram_canonical, panel, year, total_words) %>%
  summarise(total = sum(total)) %>%
  ungroup() %>%
  filter(year <= 2011, year > 1930)

lab <- plyr::ddply(temp, c("panel", "gram_canonical"), function(x) {
  xx <- filter(x, year <= 2011, year > 1930)
  m <- mgcv::gam(total/total_words*1000 ~ s(year), data = xx)
  data.frame(year = 2011, y = predict(m, newdata = data.frame(year = 2011))[[1]])
})

g <- temp %>%
  ggplot(aes(year, total/total_words*1000, group = gram_canonical,
    colour = gram_canonical)) +
  guides(colour = FALSE) +
  geom_line(alpha = 0.5, colour = "grey30") +
  stat_smooth(method = "gam", formula = y ~ s(x), se = FALSE,
    method.args = list(family = gaussian(link = "identity")), lwd = 1) +
  facet_wrap(~panel, scales = "free_y") +
  ggsidekick::theme_sleek() +
  ggrepel::geom_text_repel(data = lab,
    aes(label = gram_canonical, y = y),
    size = 3.5,
    nudge_x = 10,
    segment.size = 0.2,
    segment.color = "#00000030"
  ) +
  scale_x_continuous(breaks = seq(1920, 2012, 20), limits = c(1935, 2035))
ggsave("figs/becky-2017-12-14.pdf", width = 15, height = 14)

g <- temp %>%
  ggplot(aes(year, total/total_words*1000, group = gram_canonical,
    colour = panel)) +
  guides(colour = FALSE) +
  geom_line(alpha = 0.5, colour = "grey30") +
  stat_smooth(method = "gam", formula = y ~ s(x), se = FALSE,
    method.args = list(family = gaussian(link = "identity")), lwd = 1) +
  facet_wrap(~paste(panel, gram_canonical), scales = "free_y") +
  ggsidekick::theme_sleek()
ggsave("figs/becky-2017-12-14-separate.pdf", width = 20, height = 15)
