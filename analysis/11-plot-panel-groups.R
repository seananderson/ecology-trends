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

gold <- 0.618

# Methods: -------------------------------------------------------------------
make_handpicked_panel(
  terms_file = "data/methods-models.csv",
  cache_file = "data/generated/method-grams.rds",
  fig_file   = "figs/stats.pdf",
  fig_height = 6 * gold,
  fig_width  = 6,
  right_gap  = 54
)

make_handpicked_panel(
  terms_file = "data/methods-models-supp.csv",
  cache_file = "data/generated/method-grams.rds",
  fig_file   = "figs/stats-supp.pdf",
  fig_height = 3 * gold,
  fig_width  = 3,
  right_gap  = 54
)


# Brent: ---------------------------------------------------------------------
make_handpicked_panel(
  terms_file = "data/conservation-terms.csv",
  cache_file = "data/generated/conservation-grams.rds",
  fig_file   = "figs/conservation-panels-3.pdf",
  fig_height = 3.9,
  right_gap  = 38
)

# Paul: ----------------------------------------------------------------------
make_handpicked_panel(
  terms_file = "data/paul_human_impacts3.csv",
  cache_file = "data/generated/human-impacts-grams.rds",
  fig_file   = "figs/human-impacts-panels-4.pdf",
  fig_height = 3.6,
  right_gap  = 38
)

# Becky: ---------------------------------------------------------------------
make_handpicked_panel(
  terms_file = "data/scale_panels_2.csv",
  cache_file = "data/generated/becky-2017-12-14.rds",
  fig_file   = "figs/scale-panels-2.pdf",
  fig_height = 10,
  right_gap  = 64
)

# Genetics: ------------------------------------------------------------------
make_handpicked_panel(
  terms_file = "data/genetics-terms.csv",
  cache_file = "data/generated/genetic-ngrams.rds",
  fig_file   = "figs/genetic-panels.pdf",
  fig_height = 5,
  fig_width  = 7,
  right_gap  = 74
)

# Social Science: ------------------------------------------------------------
make_handpicked_panel(
  terms_file = "data/social-science-terms.csv",
  cache_file = "data/generated/social-science-ngrams.rds",
  fig_file   = "figs/social-science-panels.pdf",
  fig_height = 7,
  fig_width  = 7,
  right_gap  = 74
)
