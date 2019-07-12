file <- "ecograms"
file.rmd <- paste0(file, ".Rmd")
file.tex <- paste0(file, ".tex")

rmarkdown::render(file.rmd)
d <- readLines(file.tex)

figure_line <- grep("^\\\\hypertarget\\{figures", d)[[1]] - 2 # - 2 to get \clearpage
bib_beg_line <- grep("^\\\\hypertarget\\{references", d)[[1]]
bib_end_line <- length(d) - 2

d <- gsub("Fig\\. \\\\ref", "Fig\\.~\\\\ref", d)
d <- gsub("Figs \\\\ref", "Figs~\\\\ref", d)
d <- gsub("Table \\\\ref", "Table~\\\\ref", d)
d <- gsub("Tables \\\\ref", "Tables~\\\\ref", d)

d <- d[c(
  1:(figure_line - 1),
  bib_beg_line:bib_end_line,
  c(figure_line:bib_beg_line - 1), length(d) - 1, length(d)
)]
writeLines(d, file.tex)

tinytex::latexmk(file.tex)
