explore_grams <- function(dat, colour = "gram",
  colour_scale = NULL) {

  library(tidyverse)
  # library(ggsidekick)
  library(forcats)
  library(directlabels)

  if (is.null(colour_scale)) {
      if (all(is.logical(dat[,colour][[1]]))) {
        colour_scale <-
          scale_colour_manual(values = c("TRUE" = "red", "FALSE" = "grey70"))
      } else {
        colour_scale <- scale_color_discrete()
      }
  }
  dd <- filter(dat, year <= 2012, year >= 1935) %>%
    ungroup() %>%
    group_by(gram) %>%
    mutate(max_count = max(total/total_words)) %>%
    ungroup()

  dat <- ungroup(dat)

  dd <- group_by(dd, gram) %>%
    filter(total > 10) %>%
    mutate(n_year = n()) %>%
    ungroup() %>%
    filter(n_year >= 5)

  library(mgcv)

  dd2 <- plyr::ddply(dd, "gram", function(x) {
    m <- tryCatch({gam(I(log10(total/total_words*10000)) ~ s(year), data = x)},
      error = function(ignore)
        gam(I(log10(total/total_words*10000)) ~ s(year, k = 3), data = x)
    )
    mlm <- lm(I(log10(total/total_words*10000)) ~ year, data = x)
    y <- seq(min(x$year), max(x$year), 0.5)
    data.frame(year = y, pred = predict(m, newdata = data.frame(year = y)),
      slope = coef(mlm)[[2]])
  })

  dd2 <- left_join(dd2, unique(select(dat, -year, -total, -total_words)),
    by = "gram")

  labs_min <- group_by(dd2, gram) %>%
    summarise(pred = pred[year == min(year)],
      slope = slope[year == min(year)], year = min(year))
  labs_max <- group_by(dd2, gram) %>%
    summarise(pred = pred[year == max(year)],
      slope = slope[year == max(year)], year = max(year))


  labs_min <- left_join(labs_min, unique(select(dat, -year, -total, -total_words)),
    by = "gram")
  labs_max <- left_join(labs_max, unique(select(dat, -year, -total, -total_words)),
    by = "gram")

  g <- ggplot(dd2,
    aes_string("year", "pred", group = "gram", colour = colour)) +
    theme_light() +
    geom_line(lwd = 0.8) +
    ggrepel::geom_text_repel(
      data = labs_min,
      aes_string(label = "gram"),
      size = 3,
      nudge_x = -5,
      segment.size = 0.2,
      segment.color = "#00000030"
    ) +
    ggrepel::geom_text_repel(
      data = labs_max,
      aes_string(label = "gram"),
      size = 3,
      nudge_x = 10,
      segment.size = 0.2,
      segment.color = "#00000030"
    ) +
    ylab("log10(Instances per 10000 words)") +
    xlim(1925, 2025) +
    colour_scale +
    # scale_color_gradient2(
    # low = scales::muted("blue"),
    # mid = "grey60",
    # high = scales::muted("red"), guide = "none") +
    scale_alpha_continuous(limits = c(-20, -2)) +
    guides(colour = FALSE)

  g
}
