library(tidyverse)
source("analysis/plot_grams.R")
d_grams <- readRDS("data/generated/d_grams.rds")



filter(d_grams, subpanel == "conservation") %>%
  # filter(gram %in% unique(gram)[1:15]) %>%
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
  filter(gram %in% unique(gram)[1:10]) %>%
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

d_grams <- d_grams[!duplicated(d_grams), ]

ecology <- get_ngram_dat(c("ecology"))
saveRDS(ecology, file = "data/generated/ecology.rds")

ecology <- rename(ecology, ecology = total) %>%
  select(year, ecology)
d_grams <- inner_join(d_grams, ecology)

conservation <- filter(d_grams, gram == "conservation")
conservation <- rename(conservation, conservation = total) %>%
  select(year, conservation)
d_grams <- inner_join(d_grams, conservation)

maxes <- d_grams %>% group_by(gram) %>%
  filter(gram != "biomass") %>%
  filter(theme == "tools") %>%
  filter(!gram %in% c("r", "plot", "history", "resistance")) %>%
  filter(year > 2000, year <= 2011) %>%
  summarise(m = max(total/ecology)) %>%
  arrange(-m) %>%
  `[`(1:12, )

# dd <- d_grams %>%
#   filter(subpanel == "conservation", year <= 2011, year >= 1935,
#     gram %in% c("diversity", "biodiversity", "environment", "conservation", "extinction"))
# nrow(dd)

dd <- d_grams %>%
  filter(gram %in% maxes$gram) %>%
  filter(year <= 2011, year >= 1930)
nrow(dd)

dd <- dd[!duplicated(select(dd, year, gram, total)), ]
dd$gram <- as.character(dd$gram)

# c("decline", "extinction", "endangered", "red list",
  # "extirpation", "at risk", "vulnerable")

# dd3 <- filter(dd, gram %in% c("decline", "extinction", "endangered", "red list",
#   "extirpation", "at risk", "vulnerable")) %>%
#   mutate(gram2 = as.character(gram))

brent <- readr::read_csv("data/brent.csv") %>%
  rename(gram = term) %>%
  select(-subpanel, -notes) %>%
  inner_join(d_grams) %>%
  filter(year > 1935, year <= 2011)

paul <- readr::read_csv("data/paul.csv") %>%
  rename(gram = term) %>%
  select(-subpanel, -notes) %>%
  inner_join(d_grams) %>%
  filter(year > 1935, year <= 2011)

dd3 <- filter(paul, panel == "e")
# dd3 <- filter(d_grams, gram == "conservation")
dd3 <- dd3[!duplicated(select(dd3, year, gram, total)), ]
dd3 %>%
ggplot(aes(year, total/ecology, group = gram, colour = gram)) +
  # geom_vline(xintercept = seq(1940, 2012, 2), col = "grey90") +
  guides(colour = FALSE) +
  geom_line(alpha = 0.5) +
  # geom_smooth(se = FALSE) +
  stat_smooth(method = "gam", formula = y ~ s(x), se = FALSE, method.args = list(family = gaussian(link = "identity")), lwd = 2) +
  theme_light() +
  ggrepel::geom_text_repel(data = filter(dd3, year == 2011),
    aes_string(label = "gram"),
    size = 6,
    nudge_x = 10,
    segment.size = 0.2,
    segment.color = "#00000030"
  ) +
  xlim(1935, 2018) +
  # scale_y_log10() +
  scale_x_continuous(breaks = seq(1930, 2012, 10), limits = c(1930, 2018))


# -----------

# climate change, global warming
# bleaching
#

plot_ngram_all <- function(data, filename = "figs/x.pdf", width = 33,
  height = 22, order_by = "max", slope_years = c(1935:2011),
  scales = "free_y", log_y = FALSE, year_range = c(1935, 2011)) {

  data <- filter(data, year <= year_range[2], year >= year_range[1])

  if (order_by == "max") {
    data <- data %>%
      ungroup() %>% group_by(gram) n%>%
      mutate(order_column = max(total/ecology))
  }
  if (order_by == "slope") {
    data_sum <- data %>% filter(year %in% slope_years) %>%
      ungroup() %>% group_by(gram) %>%
      summarise(order_column =
          coef(lm(log(.data$total/.data$ecology) ~ .data$year))[[2]])
    data <- left_join(data, data_sum, by = "gram")
  }

  g <- data %>%
    ggplot(aes(year, (total/ecology)*1e6)) +
    geom_point(alpha = 0.7, colour = "grey40") +
    geom_line(alpha = 0.5) +
    geom_smooth(colour = "red", method = "gam",
      method.args = list(family = gaussian(link = "identity")),
      formula = y ~ s(x), se = FALSE) +
    theme_sleek() +
    facet_wrap(~fct_reorder(gram, -order_column), scales = scales) +
    ylab("Instances per million words")

  if (log_y) g <- g + scale_y_log10()

  ggsave(filename, width = width, height = height)
}

plyr::d_ply(d_grams, "theme", function(x) {
    plot_ngram_all(x, filename = paste0("figs/", unique(x$theme), ".pdf"))
})

plyr::d_ply(d_grams, "theme", function(x) {
  plot_ngram_all(x, filename = paste0("figs/scales-fixed-", unique(x$theme), ".pdf"), scales = "fixed")
})

plyr::d_ply(d_grams, "theme", function(x) {
  plot_ngram_all(x, filename = paste0("figs/order-by-slope", unique(x$theme), ".pdf"), order_by = "slope")
})



#  BLANK scale
