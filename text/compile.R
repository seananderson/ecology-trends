file <- "ecograms"
file.rmd <- paste0(file, ".Rmd")
file.tex <- paste0(file, ".tex")

rmarkdown::render(file.rmd)
