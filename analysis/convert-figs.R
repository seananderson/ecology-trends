setwd("figs")
f <- c(
  "decades-and-booms-viridis.pdf",
  "blanks-viridis5.pdf",
  "ecology-panels3.pdf",
  "conservation-human-impacts.pdf",
  "methods-models-genetics2.pdf",
  "social-science-panels3.pdf")

f_supp <- c(
  "journal-totals-by-year.pdf",
  "webfigure2.pdf",
  "community-ecology-facets.pdf",
  "conservation-human-impacts-facets.pdf",
  "blanks-extras-2018-08-09.pdf",
  "methods-models-genetics-facets.pdf",
  "social-facets.pdf",
  "booms-decline.pdf",
  "decadal-giant-1920-1959-1gram.pdf",
  "decadal-giant-1960-1979-1gram.pdf",
  "decadal-giant-1980-1999-1gram.pdf",
  "decadal-giant-2000-2010-1gram.pdf",
  "decadal-giant-1920-1959-2gram.pdf",
  "decadal-giant-1960-1979-2gram.pdf",
  "decadal-giant-1980-1999-2gram.pdf",
  "decadal-giant-2000-2010-2gram.pdf"
  )

library(future)
plan(multisession)

furrr::future_map(seq_along(f), function(i) {
  fig <- i
  this_fig.tiff <- paste0("Figure", fig, ".tiff")
  this_fig.jpg <- gsub("tiff$", "jpg", this_fig.tiff)
  command <- paste0("convert -density 1250 -background white -flatten -quality 99 -resize 50% ", f[i], " ", this_fig.jpg)
  system(command)
})

command <- paste0("convert -density 1250 -background white -flatten -quality 99 -resize 50% ", "decades-and-booms.pdf", " ", "Figure1a.jpg")
system(command)

purrr::walk(seq_along(f), function(i) {
  fig <- i
  this_fig.pdf <- paste0("Figure", fig, ".pdf")
  command <- paste0("cp ", f[i], " ", this_fig.pdf)
  system(command)
})

command <- paste0("cp ", "decades-and-booms.pdf", " ", "Figure1a.pdf")
system(command)

# furrr::future_map(seq_along(f_supp), function(i) {
furrr::future_map(seq_along(f_supp[1:8]), function(i) {
  fig <- i
  this_fig.tiff <- paste0("WebFigure", fig, ".tiff")
  this_fig.jpg <- gsub("tiff$", "jpg", this_fig.tiff)
  this_fig.jpg2 <- gsub("\\.jpg$", "-lowres.jpg", this_fig.jpg)
  command <- paste0("convert -density 1000 -background white -flatten -quality 96 -resize 33% ", f_supp[i], " ", this_fig.jpg2)
  system(command)
})

plan(sequential)
setwd("..")
