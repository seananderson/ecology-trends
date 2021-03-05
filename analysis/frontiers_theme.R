frontiers_theme <- function(base_size = 11, base_family = "") {
  half_line <- base_size / 2
  theme_light(base_size = base_size, base_family = base_family) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks.length = grid::unit(half_line / 2.2, "pt"),
      strip.background = element_rect(fill = NA, colour = NA),
      strip.text.x = element_text(colour = "black"),
      strip.text.y = element_text(colour = "black"),
      axis.text = element_text(colour = "black"),
      axis.title = element_text(colour = "black"),
      axis.ticks = element_line(colour = "grey15"),
      legend.title = element_text(
        colour = "black",
        size = rel(0.9)
      ), panel.border = element_rect(
        fill = NA,
        colour = "grey15", size = 1
      ), legend.key.size = grid::unit(
        0.9,
        "lines"
      ), legend.text = element_text(
        size = rel(0.7),
        colour = "black"
      ), legend.key = element_rect(
        colour = NA,
        fill = NA
      ), legend.background = element_rect(
        colour = NA,
        fill = NA
      ), plot.title = element_text(
        colour = "black",
        size = rel(1)
      ), plot.subtitle = element_text(
        colour = "black",
        size = rel(0.85)
      )
    )
}
