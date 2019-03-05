library("tidyverse")
source("analysis/plot-panels.R")
source("analysis/extract-functions.R")
source("analysis/pretty-panels.R")
source("analysis/make-handpicked-panel.R")

pal_func <- function(n) {
  pal <- viridisLite::plasma(n, begin = 0.01, end = 0.81, direction = -1)
  gsub("FF$", "", pal)
  # RColorBrewer::brewer.pal(n, "Dark2")
}

# Methods/Genetics: ----------------------------------------------------------
g <- make_handpicked_panel(
  terms_file = "data/methods-models-genetics2.csv",
  csv_out    = "data/generated/methods-models-genetics-out.csv",
  cache_file = "data/generated/method-models-genetics-grams3.rds",
  fig_file   = "figs/methods-models-genetics2.pdf",
  fig_height = 6.5 * 3/2 * gold(),
  fig_width  = 6.5,
  right_gap  = 77
)
ggsave("figs/methods-models-genetics-facets.pdf", width = 13, height = 18)

make_handpicked_panel(
  terms_file = "data/stats-supp.csv",
  cache_file = "data/generated/stats-supp.rds",
  fig_file   = "figs/stats-supp.pdf",
  fig_height = 3.5 * gold(),
  fig_width  = 3.5,
  right_gap  = 45,
  ncols      = 1
)
make_handpicked_panel(
  terms_file = "data/genetics-supp.csv",
  cache_file = "data/generated/genetics-supp.rds",
  fig_file   = "figs/genetics-supp.pdf",
  fig_height = 6.5 * gold() * 1 / 2,
  fig_width  = 6.5,
  right_gap  = 82
)

# Conservation and human impacts: --------------------------------------------
g <- make_handpicked_panel(
  terms_file = "data/conservation-human-impacts.csv",
  csv_out    = "data/generated/conservation-human-impacts-out.csv",
  cache_file = "data/generated/conservation-human-impacts.rds",
  fig_file   = "figs/conservation-human-impacts.pdf",
  fig_height = 6.5 * 3 / 2 * gold(),
  fig_width  = 6.5,
  right_gap  = 38
)
ggsave("figs/conservation-human-impacts-facets.pdf", width = 13, height = 18)

# General ecology: -----------------------------------------------------------
g <- make_handpicked_panel(
  terms_file = "data/ecology_panels_10_05.csv",
  csv_out    = "data/generated/general-ecology-out.csv",
  cache_file = "data/generated/ecology-panels2.rds",
  fig_file   = "figs/ecology-panels3.pdf",
  fig_height = 6.5 * gold() * 4 / 2,
  fig_width  = 6.5,
  right_gap  = 55
)
ggsave("figs/community-ecology-facets.pdf", width = 13, height = 11)

# Social Science: ------------------------------------------------------------
g <- make_handpicked_panel(
  terms_file = "data/social-ngram.csv",
  csv_out    = "data/generated/social-sciences-out.csv",
  cache_file = "data/generated/social-science-ngrams3.rds",
  fig_file   = "figs/social-science-panels3.pdf",
  fig_height = 6.5 * 2 / 2 * gold(),
  fig_width  = 6.5,
  right_gap  = 81
)
ggsave("figs/social-facets.pdf", width = 13, height = 13)
