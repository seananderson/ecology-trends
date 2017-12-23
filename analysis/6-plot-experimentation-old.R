library(tidyverse)
library(ggsidekick)
library(forcats)
library(directlabels)

gt <- readr::read_csv("data/Grouped-terms-Sheet1.csv") %>% select(-Notes) %>%
  rename(terms = Term, group = Group) %>%
  transform(terms = tolower(terms), group = tolower(group))
# dat_gt <- get_ngram_dat(gt$terms)
# saveRDS(dat_gt, file = "data/generated/grouped-terms-dat.rds")
dat_gt <- readRDS("data/generated/grouped-terms-dat.rds")

dat_gt <- left_join(dat_gt, rename(gt, gram = terms))

d <- filter(dat_gt, group == "population_biology")

dd <- filter(dat_gt, group %in% "conservation", year <= 2015, year >= 1935) %>%
  ungroup() %>%
  group_by(gram) %>%
  mutate(max_count = max(total/total_words))

dd <- group_by(dd, gram) %>%
  filter(total > 10) %>%
  mutate(n_year = n()) %>%
  ungroup() %>%
  filter(n_year >= 5)

library(mgcv)
dd2 <- plyr::ddply(dd, "gram", function(x) {
  m <- tryCatch({gam(I(log10(total/total_words*10000)) ~ s(year), data = x)},
    error = function(ignore) gam(I(log10(total/total_words*10000)) ~ s(year, k = 3), data = x)
  )
  mlm <- lm(I(log10(total/total_words*10000)) ~ year, data = x)
  y <- seq(min(x$year), max(x$year), 0.5)
  data.frame(year = y, pred = predict(m, newdata = data.frame(year = y)),
    slope = coef(mlm)[[2]])
})

labs_min <- group_by(dd2, gram) %>%
  summarise(pred = pred[year == min(year)],
    slope = slope[year == min(year)], year = min(year))
labs_max <- group_by(dd2, gram) %>%
  summarise(pred = pred[year == max(year)],
    slope = slope[year == max(year)], year = max(year))

sam <- sample(unique(dd2$gram), 5)
ggplot(filter(dd2, gram %in% sam), aes(year, pred, group = gram, colour = gram)) +
  geom_line() +
  scale_colour_discrete(guide = 'none') +
  geom_point(data = filter(dd, gram %in% sam), aes(y = log10(total/total_words*10000)))

g <- ggplot(dd2, aes(year, pred, group = gram, colour = gram)) +
  theme_sleek() +
  geom_line(lwd = 0.8) +
  ggrepel::geom_text_repel(
    data = labs_min,
    aes(label = gram),
    size = 3,
    nudge_x = -5,
    segment.size = 0.2,
    segment.color = "#00000030"
  ) +
  ggrepel::geom_text_repel(
    data = labs_max,
    aes(label = gram),
    size = 3,
    nudge_x = 10,
    segment.size = 0.2,
    segment.color = "#00000030"
  ) +
  ylab("log10(Instances per 10000 words)") +
  # ggtitle(group_name) +
  xlim(1925, 2025) +
  scale_colour_discrete(guide = 'none')
  # scale_color_gradient2(
  # low = scales::muted("blue"),
  # mid = "grey60",
  # high = scales::muted("red"), guide = "none") +
  # scale_alpha_continuous(limits = c(-20, -2))
# # viridis::scale_colour_viridis(discrete = TRUE)


g


#






plot_group_clown_vomit <- function(dat, group_name, width = 30, height = 25) {


  library(mgcv)

  dd2 <- plyr::ddply(dd, "gram", function(x) {
    m <- tryCatch({gam(I(log10(total/total_words*10000)) ~ s(year), data = x)},
      error = function(ignore) gam(I(log10(total/total_words*10000)) ~ s(year, k = 3), data = x)
    )
    mlm <- lm(I(log10(total/total_words*10000)) ~ year, data = x)
    y <- seq(min(x$year), max(x$year), 0.5)
    data.frame(year = y, pred = predict(m, newdata = data.frame(year = y)),
      slope = coef(mlm)[[2]])
  })

  labs_min <- group_by(dd2, gram) %>%
    summarise(pred = pred[year == min(year)],
      slope = slope[year == min(year)], year = min(year))
  labs_max <- group_by(dd2, gram) %>%
    summarise(pred = pred[year == max(year)],
      slope = slope[year == max(year)], year = max(year))

  g <- ggplot(dd2, aes(year, pred, group = gram, colour = gram)) +
    theme_sleek() +
    geom_line(lwd = 0.8) +
    ggrepel::geom_text_repel(
      data = labs_min,
      aes(label = gram),
      size = 3,
      nudge_x = -5,
      segment.size = 0.2,
      segment.color = "#00000030"
    ) +
    ggrepel::geom_text_repel(
      data = labs_max,
      aes(label = gram),
      size = 3,
      nudge_x = 10,
      segment.size = 0.2,
      segment.color = "#00000030"
    ) +
    ylab("log10(Instances per 10000 words)") +
    ggtitle(group_name) +
    xlim(1925, 2025) +
    scale_colour_discrete(guide = 'none') +
    # scale_color_gradient2(
    # low = scales::muted("blue"),
    # mid = "grey60",
    # high = scales::muted("red"), guide = "none") +
    scale_alpha_continuous(limits = c(-20, -2))
  # viridis::scale_colour_viridis(discrete = TRUE)
  ggsave(paste0("figs/grouped-", group_name, ".pdf"), width = width, height = height)
}

