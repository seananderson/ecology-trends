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

# Methods/Genetics: ----------------------------------------------------------
make_handpicked_panel(
  terms_file = "data/methods-models-genetics2.csv",
  cache_file = "data/generated/method-models-genetics-grams2.rds",
  fig_file   = "figs/methods-models-genetics2.pdf",
  fig_height = 3.5 * 3/2 * gold,
  fig_width  = 3.5,
  right_gap  = 58
)
make_handpicked_panel(
  terms_file = "data/stats-supp.csv",
  cache_file = "data/generated/stats-supp.rds",
  fig_file   = "figs/stats-supp.pdf",
  fig_height = 3.5 * gold,
  fig_width  = 3.5,
  right_gap  = 45,
  ncols      = 1
)
# make_handpicked_panel(
#   terms_file = "data/genetics-supp.csv",
#   cache_file = "data/generated/genetics-supp.rds",
#   fig_file   = "figs/genetics-supp.pdf",
#   fig_height = 3 * gold,
#   fig_width  = 3,
#   right_gap  = 54
# )

# Conservation and human impacts: --------------------------------------------
make_handpicked_panel(
  terms_file = "data/conservation-human-impacts.csv",
  cache_file = "data/generated/conservation-human-impacts.rds",
  fig_file   = "figs/conservation-human-impacts.pdf",
  fig_height = 6.5*3/2*gold,
  fig_width  = 6.5,
  right_gap  = 38
)

# General ecology: -----------------------------------------------------------
make_handpicked_panel(
  terms_file = "data/scale_panels_2.csv",
  cache_file = "data/generated/becky-2017-12-14.rds",
  fig_file   = "figs/scale-panels-2.pdf",
  fig_height = 10,
  right_gap  = 64
)

# Social Science: ------------------------------------------------------------
make_handpicked_panel(
  terms_file = "data/social-ngram.csv",
  cache_file = "data/generated/social-science-ngrams2.rds",
  fig_file   = "figs/social-science-panels2.pdf",
  fig_height = 6.5*3/2*gold,
  fig_width  = 6.5,
  right_gap  = 81
)
