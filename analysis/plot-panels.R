plot_panels <- function(data, filename, year_limits = c(1930, 2010),
  width = 14, height = 14, min_years = 10, palette = "Dark2") {
  library(dplyr)
  library(ggplot2)

  temp <- data %>%
    filter(show == "yes") %>%
    group_by(gram_canonical, panel, year, total_words) %>%
    summarise(total = sum(total)) %>%
    ungroup() %>%
    group_by(gram_canonical, panel) %>%
    mutate(n_years = length(unique(year))) %>%
    ungroup() %>%
    filter(year <= year_limits[2], year >= year_limits[1], n_years > min_years)

  gd <- rename(temp, panel_lemma = panel, lemma = gram_canonical)

  # Make colour palette:
  check <- group_by(gd, panel_lemma) %>% summarise(n_col = length(unique(lemma)))

  pal <- RColorBrewer::brewer.pal(
    RColorBrewer::brewer.pal.info[palette,]$maxcolors, palette)
  pal <- rep(pal, 99)[seq_len(max(check$n_col))]
  colour_scale <- scale_colour_manual(values = pal)

  make_panel <- function(dat, lab_dat, title) {
    g <- dat %>%
      filter(year <= year_limits[2], year >= year_limits[1]) %>%
      ggplot(aes(year, total/total_words*10000, group = lemma)) +
      geom_line(colour = "grey30", alpha = 0.2) +
      ylim(0, NA) +
      geom_smooth(method = "gam",
        method.args = list(family = gaussian(link = "identity")),
        formula = y ~ s(x), se = FALSE,
        aes(colour = lemma), lwd = 1.25) +
      ggsidekick::theme_sleek() +
      colour_scale +
      ggrepel::geom_text_repel(data = lab_dat,
        aes_string(y = "y", label = "lemma", colour = "as.factor(lemma)"),
        size = 3.5,
        nudge_x = 12,
        segment.size = 0.2,
        segment.color = "#00000030"
      ) +
      scale_x_continuous(breaks = seq(1920, 2012, 20), limits = c(1935, 2045)) +
      guides(colour = FALSE, lty = FALSE) +
      ylab("Instances per 10,000 words") + xlab("") +
      ggtitle(title)
  }

  out <- plyr::dlply(gd, "panel_lemma", function(xx) {
    lab <- plyr::ddply(xx, c("panel_lemma", "lemma"), function(x) {
      xx <- filter(x, year <= year_limits[2], year >= year_limits[1])
      m <- tryCatch({mgcv::gam(total/total_words*10000 ~ s(year), data = xx)},
        error = function(e) NA)
      y <- ifelse(!is.na(m)[[1]],
        predict(m, newdata = data.frame(year = year_limits[2]))[[1]], NA)
      data.frame(year = year_limits[2], y = y)
    })

    make_panel(xx, lab, unique(xx$panel))
  })

  library(gridExtra)
  pdf(filename, width = width, height = height)
  n <- length(out)
  nCol <- floor(sqrt(n))
  do.call("grid.arrange", c(out, ncol=nCol))
  dev.off()
}
