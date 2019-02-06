files <-
  c(
    "decades-and-booms-viridis.pdf",
    "blanks-viridis5.pdf",
    "ecology-panels3.pdf",
    "methods-models-genetics2.pdf",
    "conservation-human-impacts.pdf",
    "social-science-panels3.pdf",
    "booms-viridis.pdf",
    "stats-supp.pdf",
    # "ecology_supp_panels_06_19.pdf",
    "genetics-supp.pdf",
    "blanks-extras-2018-08-09.pdf"
  )

files_png <-
  c(
    "fig1.png",
    "fig2.png",
    "fig3.png",
    "fig4.png",
    "fig5.png",
    "fig6.png",
    "booms.png",
    "stats-supp.png",
    # "ecology_supp_panels_06_19.png",
    "genetics-supp.png",
    "blanks-extras-2018-08-09.png"
  )

setwd("figs")

# files_png <- sub("\\.pdf", ".png", files)

library(doParallel)
cores <- parallel::detectCores()
doParallel::registerDoParallel(cores = cores)

plyr::l_ply(seq_along(files), function(i) {
  system(paste("convert -flatten -density 380 -quality 100",
    files[i], files_png[i]))
  system(paste0("optipng -strip all ", files_png[i]))
}, .parallel = TRUE)

# files_per_core <- ceiling(length(files)*2/cores)
#
# system(paste0("find -X . -name '*.png' -print0 | xargs -0 -n ",
#   files_per_core, " -P ", cores, " optipng -strip all"))

setwd("..")

