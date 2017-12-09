library(tidyverse)
source("analysis/plot_grams.R")
d_grams <- readRDS("data/generated/d_grams.rds")

filter(d_grams, subpanel == "conservation") %>%
  plot_grams()

filter(d_grams, subpanel == "conservation") %>%
  plot_grams(colour_scale = viridis::scale_colour_viridis(discrete = TRUE))

filter(d_grams, subpanel == "conservation") %>%
  mutate(highlight = gram %in% c(
    "conservation",
    "natural history"
  )) %>%
  plot_grams(colour = "highlight")

filter(d_grams, subpanel == "conservation") %>%
  mutate(highlight = gram %in% c(
    "allee effect",
    "diversity"
  )) %>%
  plot_grams(colour = "highlight",
    colour_scale =
      scale_colour_manual(values = c("TRUE" = "red", "FALSE" = "grey60")))

filter(d_grams, subpanel == "conservation") %>%
  mutate(highlight = case_when(
    gram %in% c(
      "conservation",
      "natural history") ~ "a",
    gram %in% c("diversity") ~ "b",
    TRUE ~ "c")) %>%
  plot_grams(colour = "highlight",
    colour_scale = scale_colour_manual(values =
        c(
          "a" = "red",
          "b" = "blue",
          "c" = "grey70")))
